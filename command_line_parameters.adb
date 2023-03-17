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

with Ada.Command_Line;
with Generic_Line_Parser;
with Ada.Strings.Fixed;
with Ada.Exceptions;
with Sectioned_Text;
with Line_Arrays;
with Ada.Text_IO;

package body Command_Line_Parameters is
   use Package_Names;

   package Line_Parsers is
      new Generic_Line_Parser (Program_Parameters);

   generic
      type Element_Type (<>) is private;

      with package Element_Lists is
         new Ada.Containers.Indefinite_Doubly_Linked_Lists (Element_Type);
   procedure Append (To   : in out Element_Lists.List;
                     Item : in     Element_Lists.List);

   procedure Append (To   : in out Element_Lists.List;
                     Item : in     Element_Lists.List) is
      procedure Do_It (Pos : Element_Lists.Cursor) is
      begin
         To.Append (Element_Lists.Element (Pos));
      end Do_It;
   begin
      Item.Iterate (Do_It'Access);
   end Append;

   procedure Append_Package_List is
      new Append (Element_Type  => Package_Name,
                  Element_Lists => Package_Lists);

   --  To be honest, I do not love this type of shorthand...
   --  but in this package is really handy.
   function "+" (X : String) return Unbounded_String
                 renames To_Unbounded_String;

   function "+" (X : Unbounded_String) return String
                 renames To_String;

   -------------------------
   -- Input_Name_Callback --
   -------------------------

   procedure Input_Name_Callback (Name   : in     Unbounded_String;
                                  Value  : in     Unbounded_String;
                                  Result : in out Program_Parameters)
   is
      pragma Unreferenced (Name);
   begin
      Result.Input_Name.Append (To_String (Value));
   end Input_Name_Callback;

   -------------------------
   -- Style_Spec_Callback --
   -------------------------

   procedure Style_Spec_Callback (Name   : in     Unbounded_String;
                                  Value  : in     Unbounded_String;
                                  Result : in out Program_Parameters)
   is
      use Style_Table;

      pragma Unreferenced (Name);
   begin
      Ada.Text_IO.Put_Line ("SPEC: " & To_String (Value));
      Result.Style_Spec.Append (Style_Spec (To_String (Value)));
   end Style_Spec_Callback;

   ---------------------------
   -- Input_Format_Callback --
   ---------------------------

   procedure Input_Format_Callback (Name   : in     Unbounded_String;
                                    Value  : in     Unbounded_String;
                                    Result : in out Program_Parameters)
   is
      pragma Unreferenced (Name);
   begin
      Result.Input_Format := Value;
   end Input_Format_Callback;

   ---------------------------
   -- Output_Format_Callback --
   ---------------------------

   procedure Output_Format_Callback (Name   : in     Unbounded_String;
                                    Value  : in     Unbounded_String;
                                    Result : in out Program_Parameters)
   is
      pragma Unreferenced (Name);
   begin
      Result.Output_Format := Value;
   end Output_Format_Callback;

   -------------------
   -- Trim_Callback --
   -------------------

   procedure Trim_Callback (Name   : in     Unbounded_String;
                            Value  : in     Unbounded_String;
                            Result : in out Program_Parameters)
   is
      pragma Unreferenced (Name);
   begin
      Append_Package_List (Result.Trim_Nodes, Parse_Package_List (+Value));
   end Trim_Callback;

   ---------------------
   -- Ignore_Callback --
   ---------------------

   procedure Ignore_Callback (Name   : in     Unbounded_String;
                              Value  : in     Unbounded_String;
                              Result : in out Program_Parameters)
   is
      pragma Unreferenced (Name);
   begin
      Append_Package_List (Result.Ignore, Parse_Package_List (+Value));
   end Ignore_Callback;

   procedure Config_Callback (Name   : in     Unbounded_String;
                              Value  : in     Unbounded_String;
                              Result : in out Program_Parameters)
   is
      pragma Unreferenced (Name);

      use Sectioned_Text;
      use Line_Arrays;

      procedure Iterate_Over_Section
        (Sections : Section_List;
         Name     : String;
         Process  : not null access procedure (Pos : Line_Containers.Cursor))
      is
      begin
         if Sections.Contains (Name) then
            Sections.Element (Name).Iterate (Process);
         end if;
      end Iterate_Over_Section;

      Classifier : constant Basic_Line_Classifier :=
                     New_Classifier (Head          => "[",
                                     Tail          => "]",
                                     Comments      => "#",
                                     Trim_Lines    => True,
                                     Trim_Names    => True,
                                     Trim_Comments => True,
                                     Ignore_Empty  => True);

      Sections : constant Section_List := Parse (+Value, Classifier);
   begin
      declare
         procedure Append (Pos : Line_Containers.Cursor) is
         begin
            Result.Ignore.Append (Package_Name (Line_Containers.Element (Pos)));
         end Append;
      begin
         Iterate_Over_Section (Sections, "ignore", Append'Access);
      end;

      declare
         procedure Append (Pos : Line_Containers.Cursor) is
         begin
            Result.Trim_Nodes.Append (Package_Name (Line_Containers.Element (Pos)));
         end Append;
      begin
         Iterate_Over_Section (Sections, "trim", Append'Access);
      end;

      declare
         procedure Append (Pos : Line_Containers.Cursor) is
            use Style_Table;
         begin
            Result.Style_Spec.Append (Style_Spec (Line_Containers.Element (Pos)));
         end Append;
      begin
         Iterate_Over_Section (Sections, "style", Append'Access);
      end;

      declare
         procedure Append (Pos : Line_Containers.Cursor) is
         begin
            Result.Input_Name.Append (Line_Containers.Element (Pos));
         end Append;
      begin
         Iterate_Over_Section (Sections, "input", Append'Access);
      end;

   end Config_Callback;

   Syntax : constant Line_Parsers.Parameter_Descriptor_Array :=
              ((Name      => +"input",
                Default    => +"",
                If_Missing => Line_Parsers.Ignore,
                Only_Once  => False,
                Callback   => Input_Name_Callback'Access),

               (Name       => +"input-format,format,in-fmt,infmt",
                Default    => +"ali",
                If_Missing => Line_Parsers.Use_Default,
                Only_Once  => True,
                Callback   => Input_Format_Callback'Access),

               (Name       => +"output-format,out-fmt,outfmt",
                Default    => +"dot",
                If_Missing => Line_Parsers.Use_Default,
                Only_Once  => True,
                Callback   => Output_Format_Callback'Access),

               (Name       => +"style",
                Default    => +"",
                If_Missing => Line_Parsers.Ignore,
                Only_Once  => False,
                Callback   => Style_Spec_Callback'Access),

               (Name       => +"trim",
                Default    => +"",
                If_Missing => Line_Parsers.Ignore,
                Only_Once  => True,
                Callback   => Trim_Callback'Access),

               (Name       => +"ignore",
                Default    => +"",
                If_Missing => Line_Parsers.Ignore,
                Only_Once  => True,
                Callback   => Ignore_Callback'Access),

               (Name       => +"config",
                Default    => +"",
                If_Missing => Line_Parsers.Ignore,
                Only_Once  => False,
                Callback   => Config_Callback'Access));

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   function Parse_Command_Line return Program_Parameters is
      use Ada.Strings;
      use Ada.Command_Line;

      Result : Program_Parameters;
   begin
      Result := (Input_Name    => <>,
                 Style_Spec    => Style_Table.Style_Spec_Lists.Empty_List,
                 Input_Format  => +"ali",
                 Output_Format => +"dot",
                 Trim_Nodes    => Package_Names.Package_Lists.Empty_List,
                 Ignore        => Package_Names.Package_Lists.Empty_List);

      if Argument_Count = 1 and then Fixed.Index (Argument (1), "=") = 0 then
         declare
            Arg : constant String := Argument (1);
         begin
            if Arg (Arg'First) /= '@' then
               Result.Input_Name.Append (Argument (1));
            else
               Config_Callback (Name   => Null_Unbounded_String,
                                Value  => +Arg (Arg'First + 1 .. Arg'Last),
                                Result => Result);
            end if;
         end;
      else
         Line_Parsers.Parse_Command_Line (Parameters  => Syntax,
                                          Result      => Result);

         Result.Ignore := Force_Prefix_Property (Result.Ignore);
         Result.Trim_Nodes := Force_Prefix_Property (Result.Trim_Nodes);
      end if;

      if Result.Input_Name.Is_Empty then
         raise User_Error with "Missing input directory(s)";
      end if;

      return Result;
   exception
      when E : Line_Parsers.Bad_Command =>
         raise User_Error with Ada.Exceptions.Exception_Message (E);
   end Parse_Command_Line;

end Command_Line_Parameters;

--        function Has_Ancestor (Container : Token_List;
--                               Node      : Unbounded_String)
--                                  return Boolean
--        is
--           use Ada.Strings.Unbounded;
--
--           Len : Natural := Length (Node);
--        begin
--           for I in Container.First_Index .. Container.Last_Index loop
--              Len := Length (Container.Element (I));
--              if  Len < Length (Node) and then
--                Slice (Node, 1, Len + 1) = Container.Element (I) & '.'
--              then
--                 return True;
--              end if;
--           end loop;
--
--           return False;
--        end Has_Ancestor;
