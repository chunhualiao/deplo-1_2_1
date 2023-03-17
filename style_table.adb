-------------------------------------------------------------------------------
--            DEpendency PLOtter for ada packages (DePlo)                    --
--                                                                           --
--               Copyright (C) 2012, Riccardo Bernardini                     --
--                                                                           --
--      This file is part of DePlo.                                          --
--                                                                           --
--      DePlo is free software: you can redistribute it and/or modify        --
--      it under the terms of the GNU General Public License as published by --
--      the Free Software Foundation, either version 2 of the License, or    --
--      (at your option) any later version.                                  --
--                                                                           --
--      DePlo is distributed in the hope that it will be useful,             --
--      but WITHOUT ANY WARRANTY; without even the implied warranty of       --
--      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        --
--      GNU General Public License for more details.                         --
--                                                                           --
--      You should have received a copy of the GNU General Public License    --
--      along with DePlo.  If not, see <http://www.gnu.org/licenses/>.       --
-------------------------------------------------------------------------------

with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Text_IO;
with Command_Line_Parameters;
package body Style_Table is

   function To_U (X : String) return Unbounded_String
                  renames To_Unbounded_String;
   pragma Unreferenced (To_U);

   function To_S (X : Unbounded_String) return String
      renames To_String;

--     type Style_Pair is
--        record
--           Name  : Unbounded_String;
--           Value : Unbounded_String;
--        end record;
--
--     function "=" (X, Y : Style_Pair) return Boolean is
--     begin
--        return X.Name = Y.Name and X.Value = Y.Value;
--     end "=";
--
--     package Style_Pair_Lists is
--        new Ada.Containers.Doubly_Linked_Lists (Style_Pair);

   function Attribute_Name_Hash (X : Attribute_Name) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (String (X));
   end Attribute_Name_Hash;

   package Attribute_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Attribute_Name,
        Element_Type    => Attribute_Value,
        Hash            => Attribute_Name_Hash,
        Equivalent_Keys => "=",
        "="             => "=");

   function Class_Hash (X : Node_Class) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash_Case_Insensitive (String (X));
   end Class_Hash;

   function Class_Equal (X, Y : Node_Class) return Boolean is
   begin
      return Ada.Strings.Equal_Case_Insensitive (String (X), String (Y));
   end Class_Equal;

   package Style_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
         (Key_Type        => Node_Class,
          Element_Type    => Attribute_Maps.Map,
          Hash            => Class_Hash,
          Equivalent_Keys => Class_Equal,
          "="             => Attribute_Maps."=");

   Table : Style_Maps.Map;
   ---------
   -- Add --
   ---------

   procedure Add
     (Class  : Node_Class;
      Name   : Attribute_Name;
      Value  : Attribute_Value;
      Append : Boolean := False)
   is
      use Style_Maps;

      Pos : Cursor := Table.Find (Class);
      Ignored : Boolean;
   begin
      if Pos = No_Element then
         Table.Insert (Key      => Class,
                       New_Item => Attribute_Maps.Empty_Map,
                       Position => Pos,
                       Inserted => Ignored);
      end if;

      declare
         procedure Update (Key  : Node_Class;
                           Item : in out Attribute_Maps.Map) is
            pragma Unreferenced (Key);
         begin
            if Item.Contains (Name) and Append then
               raise Command_Line_Parameters.User_Error with "Append unimplemented";
            else
               Item.Include (Key      => Name,
                             New_Item => Value);
            end if;
         end Update;
      begin
         Table.Update_Element (Pos, Update'Access);
      end;
   end Add;

   procedure Extract_Spec_Data (Input  : in     Style_Spec;
                                Cursor : in out Natural;
                                Class  :    out Unbounded_String;
                                Name   :    out Unbounded_String;
                                Value  :    out Unbounded_String;
                                Append :    out Boolean)
   is
      use Ada.Strings;

      function End_Of_Input return Boolean is
      begin
         return Cursor > Input'Last;
      end End_Of_Input;

      function Peek return Character is
      begin
         if End_Of_Input then
            raise Parsing_Error;
         end if;

         return Input (Cursor);
      end Peek;

      procedure Next is
      begin
         Cursor := Cursor + 1;
      end Next;

      function Get_Char return Character is
         Result : constant Character := Peek;
      begin
         Next;
         return Result;
      end Get_Char;

      function Current_Equal (Ch : Character) return Boolean is
      begin
         return not End_Of_Input and then Peek = Ch;
      end Current_Equal;

      function Current_Not_Equal (Ch : Character) return Boolean is
      begin
         return (not End_Of_Input) and then Peek /= Ch;
      end Current_Not_Equal;

      procedure Skip_Spaces is
      begin
         while Current_Equal (' ') loop
            Next;
         end loop;
      end Skip_Spaces;

      procedure Extract_Class is
      begin
         Class := Null_Unbounded_String;

         while Peek /= '-' loop
            Class := Class & Get_Char;
         end loop;
         Next;

         Unbounded.Trim (Class, Both);
      end Extract_Class;

      procedure Extract_Name is
      begin
         Name  := Null_Unbounded_String;

         if End_Of_Input then
            raise Parsing_Error;
         end if;

         while Current_Not_Equal ('=')
           and Current_Not_Equal (';')
           and Current_Not_Equal ('&')
         loop
            Name := Name & Get_Char;
         end loop;

         Unbounded.Trim (Name, Both);
      end Extract_Name;

      procedure Extract_Value is
         procedure Extract_String_Value is
         begin
            loop
               if End_Of_Input then
                  --  The input terminates inside a "..."
                  raise Parsing_Error with "Unterminated string";
               end if;

               if Peek = '"' then
                  --  Current char is a '"'.  This terminates the string
                  --  unless next char is a '"' too.  Note that the string
                  --  ends also if the current '"' is the last character
                  --  of the input.
                  Next;

                  if End_Of_Input or else Peek /= '"' then
                     exit;
                  end if;
               end if;

               Value := Value & Get_Char;
            end loop;
         end Extract_String_Value;
      begin
         Value := Null_Unbounded_String;
         Append := False;

         if Current_Equal (';') then
            Next;
            return;

         elsif Current_Equal ('=') or Current_Equal ('&') then
            if Current_Equal ('&') then
               Next;
               Append := True;

               if not Current_Equal ('=') then
                  raise Parsing_Error with "'&' without '='";
               end if;
            end if;

            --  When I am here Current_Equal('=') is True
            Next;

            if Current_Equal ('"') then
               Next;

               Extract_String_Value;

               --  Skip optional spaces after the last '"'
               Skip_Spaces;

               --  Now the next char, if it exists, can only be a ';'
               if not End_Of_Input then
                  if Peek /= ';' then
                     raise Parsing_Error;
                  else
                     Next;
                  end if;
               end if;
            else
               while Current_Not_Equal (';') loop
                  Value := Value & Get_Char;
               end loop;

               if Current_Equal (';') then
                  Next;
               end if;

               Unbounded.Trim (Value, Both);
            end if;
         end if;
      end Extract_Value;
   begin
      if Cursor = 0 then
         Cursor := Input'First;
      end if;

      Skip_Spaces;

      if End_Of_Input then
         Class := Null_Unbounded_String;
         return;
      end if;

      Extract_Class;
      Extract_Name;
      Extract_Value;
   end Extract_Spec_Data;

   -------------------
   -- Parse_And_Add --
   -------------------

   procedure Parse_And_Add (Spec : Style_Spec) is
      use Ada.Strings;

      Cursor : Natural;

      Class  : Unbounded_String;
      Name   : Unbounded_String;
      Value  : Unbounded_String;
      Append : Boolean;
   begin
      Cursor := 0;

      loop
         Extract_Spec_Data (Input  => Spec,
                            Cursor => Cursor,
                            Class  => Class,
                            Name   => Name,
                            Value  => Value,
                            Append => Append);

         exit when Class = Null_Unbounded_String;

--           Ada.Text_IO.Put_Line ("name=" & To_String (Name) & "append=" & Append'Img);

         Add (Class  => Node_Class (To_S (Class)),
              Name   => Attribute_Name (To_S (Name)),
              Value  => Attribute_Value (To_S (Value)),
              Append => Append);
      end loop;
   end Parse_And_Add;

   -------------------
   -- Parse_And_Add --
   -------------------

   procedure Parse_And_Add (Specs : Style_Spec_Lists.List) is
      procedure Add_Single (Pos : Style_Spec_Lists.Cursor) is
      begin
         Parse_And_Add (Style_Spec_Lists.Element (Pos));
      end Add_Single;
   begin
      Specs.Iterate (Add_Single'Access);
   end Parse_And_Add;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Class   : Node_Class;
      Process : not null access procedure (Name  : Attribute_Name;
                                           Value : Attribute_Value))
   is
      procedure Dispatch (Pos : Attribute_Maps.Cursor) is
      begin
         Process (Name  => Attribute_Maps.Key (Pos),
                  Value => Attribute_Maps.Element (Pos));
      end Dispatch;
   begin
--        Ada.Text_IO.Put_Line ("Iterate : " & String (Class) & "->" & Boolean'Image (Table.Contains (Class)));
      if Table.Contains (Class) then
         Table.Element (Class).Iterate (Dispatch'Access);
      end if;
   end Iterate;

   procedure Dump (List : Style_Spec_Lists.List)
   is
      procedure Print (Pos : Style_Spec_Lists.Cursor) is
      begin
         Ada.Text_IO.Put_Line (String (Style_Spec_Lists.Element (Pos)));
      end Print;
   begin
      List.Iterate (Print'Access);
   end Dump;

end Style_Table;

--     procedure Parse_And_Add (Spec : Style_Spec) is
--
--        use Ada.Strings;
--        use Ada.Strings.Fixed;
--
--        function Trim (X : Style_Spec) return String is
--        begin
--           return Trim (String (X), Both);
--        end Trim;
--        pragma Inline (Trim);
--
--        Idx_Colon : Natural := Index (String (Spec), "-");
--        Idx_Equal : Natural := Index (String (Spec), ":");
--     begin
--        if Idx_Colon <= Spec'First or Idx_Colon = Spec'Last then
--           raise Parsing_Error with "Missing '-' in spec";
--        end if;
--
--        if Idx_Equal <= Idx_Colon + 1 then
--           raise Parsing_Error with "Missing '=' in spec";
--        end if;
--
--        Add (Class       => Node_Class (Trim (Spec (Spec'First .. Idx_Colon - 1))),
--             Style_Name  => Attribute_Name (Trim (Spec (Idx_Colon + 1 .. Idx_Equal - 1))),
--             Style_Value => Attribute_Value (Trim (Spec (Idx_Equal + 1 .. Spec'Last))));
--     end Parse_And_Add;
