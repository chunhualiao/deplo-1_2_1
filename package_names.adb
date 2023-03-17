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
with Tokenize;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Characters.Latin_1;
with Ada.Text_IO;

package body Package_Names is
   use Ada.Strings.Unbounded;

--     function "+" (X : String) return Unbounded_String
--                   renames To_Unbounded_String;

   function "+" (X : Unbounded_String) return String
                 renames To_String;

   ---------------------------
   -- Is_Valid_Package_Name --
   ---------------------------

   function Is_Valid_Package_Name (Name : String) return Boolean
   is
      use Ada.Strings.Maps;
      use Ada.Strings.Maps.Constants;
      use Ada.Strings.Fixed;

      subtype Fragment is String (1 .. 2);
      type Fragment_Array is
         array (Positive range <>) of Fragment;

      Invalid_Fragments : constant Fragment_Array := ("__", "_.", "._", "..");

      Valid_Set : constant Character_Set :=
                    Letter_Set or Decimal_Digit_Set or To_Set ("_.");
   begin
      if Name'Length = 0
         or else (not Is_In (Name (Name'First), Letter_Set))
         or else Index (Name, Valid_Set, Ada.Strings.Outside) /= 0
      then
         return False;
      end if;

      for I in Invalid_Fragments'Range loop
         if Index (Source  => Name,
                   Pattern => Invalid_Fragments (I)) /= 0 then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid_Package_Name;

   -------------------------
   -- Remove_Redundancies --
   -------------------------

   function Force_Prefix_Property (Input : Package_List)
                                 return Package_List is
      procedure Print_Warning (Removed : Package_List) is
         use Ada.Text_IO;
         First_Time : Boolean := True;

         procedure Print (Pos : Package_Lists.Cursor) is
         begin
            if not First_Time then
               Put (Standard_Error, ", ");
            end if;

            Put (Standard_Error, String (Package_Lists.Element (Pos)));

            First_Time := False;
         end Print;
      begin
         Put (Standard_Error, "Warning: redundant trim nodes removed (");
         Removed.Iterate (Print'Access);
         Put_Line (Standard_Error, ")");
      end Print_Warning;

      Result     : Package_List;
      Removed    : Package_List;
   begin
      declare
         -------------
         -- Process --
         -------------

         procedure Process (Pos : Package_Lists.Cursor) is
            Name : constant Package_Name := Package_Lists.Element (Pos);
         begin
            if Package_Names.Find_Ancestor (Input, Name) /= "" then
               Removed.Append (Name);
            else
               Result.Append (Name);
            end if;
         end Process;
      begin
         Input.Iterate (Process'Access);
      end;

      if not Removed.Is_Empty then
         Print_Warning (Removed);
      end if;

      return Result;
   end Force_Prefix_Property;

   -----------------------
   -- Read_Package_List --
   -----------------------

   function Read_Package_List (Filename : String) return Package_List is
      use Ada.Text_IO;
      use Ada.Strings;
      use Ada.Strings.Maps;
      use Ada.Characters;

      Spaces : constant Character_Set := To_Set (" " & Latin_1.HT);
      List   : Package_List;
      Input  : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (File => Input,
                        Mode => Ada.Text_IO.In_File,
                        Name => Filename);

      while not Ada.Text_IO.End_Of_File (Input) loop
         declare
            Line : constant String := Fixed.Trim (Source => Get_Line (Input),
                                                  Left   => Spaces,
                                                  Right  => Spaces);
         begin
            if Line'Length >= 1 and then Line (Line'First) /= '#' then
               if Is_Valid_Package_Name (Line) then
                  List.Append (Package_Name (Line));
               else
                  raise Program_Error
                     with "Invalid package name '" & Line & "'";
               end if;
            end if;
         end;
      end loop;

      Ada.Text_IO.Close (Input);

      return List;
   end Read_Package_List;

   ----------------------
   -- Get_Package_List --
   ----------------------

   function Parse_Package_List (Param : String)
                              return Package_List
   is
      use Tokenize;
      use Ada.Strings;

      function To_Package_List (Input : Token_List) return Package_List is
         Result : Package_List;
      begin
         declare
            procedure Append (Pos : String_Vectors.Cursor) is
               V : constant String := Fixed.Trim (+String_Vectors.Element (Pos), Both);
            begin
               if Is_Valid_Package_Name (V) then
                  Result.Append (Package_Name (V));
               else
                  raise Program_Error
                     with "Invalid package name '" & V & "'";
               end if;
            end Append;
         begin
            Input.Iterate (Append'Access);
         end;

         return Result;
      end To_Package_List;

   begin
      if Param (Param'First) = '@' then
         return Read_Package_List (Param (Param'First + 1 .. Param'Last));
      else
         return To_Package_List (Split (Param, ','));
      end if;
   end Parse_Package_List;

   ------------------
   -- Is_Ascendant --
   ------------------

   function Is_Ancestor
     (Ancestor  : Package_Name;
      Descendant : Package_Name)
      return Boolean
   is
      use Ada.Strings.Fixed;
   begin
      if Ancestor'Length >= Descendant'Length then
         return False;
      end if;

      return
        Descendant (Descendant'First .. Descendant'First + Ancestor'Length) =  Ancestor & ".";
   end Is_Ancestor;

   --------------------
   -- Find_Ascendant --
   --------------------

   function Find_Ancestor
     (List       : Package_List;
      Descendant : Package_Name)
      return Package_Name
   is
      use Package_Lists;

      Pos : Cursor;
   begin
      Pos := List.First;
      while Pos /= No_Element loop
         if Is_Ancestor (Ancestor  => Element (Pos), Descendant => Descendant) then
            return Element (Pos);
         end if;

         Next (Pos);
      end loop;

      return "";
   end Find_Ancestor;

   -----------------------
   -- To_Component_List --
   -----------------------

   function To_Component_List (Item : Package_Name) return Component_List is
      use Ada.Strings.Fixed;

      Result : Component_List;
      First  : Positive := Item'First;
      Pos    : Natural;
   begin
      loop
         Pos := Index (Source  => String (Item (First .. Item'Last)),
                       Pattern => ".");

         if Pos = 0 then
            Result.Append (Item (First .. Item'Last));
            exit;
         elsif Pos < First + 1 or Pos = Item'Last then
            raise Program_Error;
         else
            Result.Append (Item (First .. Pos - 1));
            First := Pos + 1;
         end if;
      end loop;

      return Result;
   end To_Component_List;

   -------------
   -- To_Name --
   -------------

   function To_Name (L : Component_List) return Package_Name is
      use Package_Lists;

      First_Time : Boolean := True;

      Result     : Unbounded_String;

      procedure Append (Pos : Package_Lists.Cursor) is
      begin
         if not First_Time then
            Result := Result & ".";
         end if;

         First_Time := False;
         Result := Result & String (Package_Lists.Element (Pos));
      end Append;
   begin
      L.Iterate (Append'Access);

      return Package_Name (To_String (Result));
   end To_Name;

end Package_Names;
