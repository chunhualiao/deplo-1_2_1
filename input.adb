----------------------------------------------------------------------------
--            DEpendency PLOtter for ada packages (DePlo)
--
--               Copyright (C) 2012, Riccardo Bernardini
--
--      This file is part of DePlo.
--
--      DePlo is free software: you can redistribute it and/or modify
--      it under the terms of the GNU General Public License as published by
--      the Free Software Foundation, either version 2 of the License, or
--      (at your option) any later version.
--
--      DePlo is distributed in the hope that it will be useful,
--      but WITHOUT ANY WARRANTY; without even the implied warranty of
--      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--      GNU General Public License for more details.
--
--      You should have received a copy of the GNU General Public License
--      along with DePlo.  If not, see <http://www.gnu.org/licenses/>.
----------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Package_Trees;
with Dependency_Graphs;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Strings.Maps;
with Ada.Text_IO;
with Ada.Directories;
with Package_Names;
with Ada.Strings.Equal_Case_Insensitive;

package body Input is
   use Package_Trees;
   use Package_Names;
   use Dependency_Graphs;

   type Filename_Type is new String;

   package Filename_Lists is
      new Ada.Containers.Indefinite_Doubly_Linked_Lists (Filename_Type);

   ---------------------------
   -- Get_Package_Structure --
   ---------------------------

   function Filename_To_Package (Item : Filename_Type) return Package_Name is
      use Ada.Strings;

      Dash_To_Dot : constant Maps.Character_Mapping := Maps.To_Mapping (From => "-",
                                                               To   => ".");

      Basename    : constant String := Ada.Directories.Base_Name (String (Item));
   begin
      return Package_Name (Fixed.Translate (Source  => String (Basename),
                                            Mapping => Dash_To_Dot));
   end Filename_To_Package;

   function Get_Dependency_Graph (DB        : Package_Trees.Package_Tree;
                                   Filenames : Filename_Lists.List)
                                   return Graph_Access
   is
      use Filename_Lists;

      function Extract_Dependencies (Filename : Filename_Type) return Package_Lists.List is
         use Ada.Text_IO;
         use Ada.Strings;

         function Parse_W_Line (Line : String) return Package_Name is
            Idx : Natural;
         begin
            if not (Line'Length > 0 and then Line (Line'First) = 'W') then
               return "";
            else
               Idx := Fixed.Index (Source  => Line,
                                    Pattern => "%");

               if Idx < Line'First + 2 then
                  raise Program_Error with "% not found in '" & Line & "'";
               end if;

               return Package_Name (Line (Line'First + 2 .. Idx - 1));
            end if;
         end Parse_W_Line;

         Result : Package_Lists.List;
         Input  : File_Type;
      begin
         Open (File => Input,
               Mode => In_File,
               Name => String (Filename));

         while not End_Of_File (Input) loop
            declare
               Pkg_Name : constant Package_Name := Parse_W_Line (Get_Line (Input));
            begin
               if DB.Contains (Pkg_Name) then
                  Result.Append (Pkg_Name);
               end if;
            end;
         end loop;

         Close (Input);
         return Result;
      end Extract_Dependencies;

      Result : constant Graph_Access := Create (DB.Size);

      procedure Process_One_ALI (Position : Filename_Lists.Cursor) is
         Wither : Package_Trees.Cursor;
         Filename : constant Filename_Type := Filename_Lists.Element (Position);
         Dependencies : Package_Lists.List;

         procedure Add_Dependency (Pos : Package_Lists.Cursor) is
            Withed : Package_Trees.Cursor;
         begin
            Withed := DB.Find (Package_Lists.Element (Pos));
            Result.Add_Dependency (Wither => Wither,
                                   Withed => Withed);
         end Add_Dependency;

      begin
         Wither := DB.Find (Filename_To_Package (Filename));
         Dependencies := Extract_Dependencies (Filename);

         Dependencies.Iterate (Add_Dependency'Access);
      end Process_One_ALI;

   begin
      Filenames.Iterate (Process_One_ALI'Access);

      return Result;
   end Get_Dependency_Graph;

   procedure Populate_Tree (DB        : in out Package_Trees.Package_Tree;
                          Filenames :    out Filename_Lists.List;
                          Base_Dir  : in     String)
   is
      use Ada.Directories;

      procedure Process (Directory_Entry : in Directory_Entry_Type) is
         function Is_Compiler_Generated (Name : String) return Boolean is
            use Ada.Strings;
         begin
            return Fixed.Index (Name, "~") /= 0 or else Fixed.Index (Name, "__") /= 0;
         end Is_Compiler_Generated;
      begin
         if not Is_Compiler_Generated (Simple_Name (Directory_Entry)) then
            DB.Add (Filename_To_Package (Filename_Type (Simple_Name (Directory_Entry))));
            Filenames.Append (Filename_Type (Full_Name (Directory_Entry)));
         end if;
      end Process;
   begin
      Search (Directory => Base_Dir,
              Pattern   => "*.ali",
              Filter    => (Directory     => False,
                            Ordinary_File => True,
                            Special_File  => False),
              Process   => Process'Access);
   end Populate_Tree;

   ---------------------------
   -- Get_Package_Structure --
   ---------------------------

   function Get_Package_Structure (Format      : String;
                                   Source_Name : String_Lists.List)
                                   return Package_DB.DB_Type
   is
      pragma Unreferenced (Format);

      Filenames : Filename_Lists.List;
      Result : Package_DB.DB_Type;
   begin
      declare
         procedure Process_Dir (Pos : String_Lists.Cursor) is
         begin
            Populate_Tree (DB        => Result.Tree,
                           Filenames => Filenames,
                           Base_Dir  => String_Lists.Element (Pos));
         end Process_Dir;
      begin
         Source_Name.Iterate (Process_Dir'Access);
      end;

      if Result.Tree.Size = 0 then
         raise Command_Line_Parameters.User_Error  with "Empty graph. No .ali file?";
      end if;

      Result.Graph := Get_Dependency_Graph (DB        => Result.Tree,
                                            Filenames => Filenames);

      return Result;
   end Get_Package_Structure;

   --------------
   -- Is_Known --
   --------------

   function Is_Known (Format : String) return Boolean is
   begin
      return Ada.Strings.Equal_Case_Insensitive (Format, "ali");
   end Is_Known;
end Input;
