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
with Ada.Strings.Fixed;

package body Sectioned_Text is

   function To_S (X : Unbounded_String) return String
                  renames To_String;

   function To_U (X : String) return Unbounded_String
                  renames To_Unbounded_String;

   function Parse
     (Lines      : Line_Array;
      Classifier : Abstract_Classifier'Class;
      Skip       : Integer := 0)
      return Section_List
   is
      use type Ada.Containers.Count_Type;

      procedure Store (Where : in out Section_List;
                       Name  : in     Unbounded_String;
                       Item  : in     Line_Array)
      is
      begin
         if Name = Null_Unbounded_String and Item.Is_Empty then
            return;
         else
            Where.Insert (To_S (Name), Item);
         end if;
      end Store;

      Result       : Section_List;
      Section_Name : Unbounded_String := Null_Unbounded_String;
      Current_Sect : Line_Array;
   begin
      for I in Skip + Lines.First_Index .. Lines.Last_Index loop
         declare
            Classified : constant Classified_Line := Classifier.Classify (Lines.Element (I));
         begin
            case Classified.Class is
               when Section =>
                  Store (Result, Section_Name, Current_Sect);
                  Current_Sect.Clear;
                  Section_Name := Classified.Name;
               when Normal =>
                  Current_Sect.Append (To_S (Classified.Content));
               when Comment =>
                  null;
            end case;
         end;
      end loop;

      Store (Result, Section_Name, Current_Sect);

      return Result;
   end Parse;

   function Parse
     (Filename    : String;
      Classifier  : Abstract_Classifier'Class;
      Skip        : Integer := 0)
      return Section_List
   is
   begin
      return Parse (Line_Arrays.Read (Filename), Classifier, Skip);
   end Parse;

      -----------
   -- Parse --
   -----------

   function Parse (Lines : Line_Array) return Section_List
   is
      Separator : constant String := Lines.First_Element;

      Classifier : constant Basic_Line_Classifier := New_Classifier (Head => Separator);
   begin
      return Parse (Lines      => Lines,
                    Classifier => Classifier,
                    Skip       => 1);
   end Parse;

   ----------------
   -- Parse_File --
   ----------------

   function Parse (Filename : String) return Section_List is
      use Ada;
   begin
      return Parse (Line_Arrays.Read (Filename));
   end Parse;

   ----------------
   -- New_Parser --
   ----------------

   function New_Classifier (Head          : String;
                            Tail          : String := "";
                            Comments      : String := "#";
                            Trim_Lines    : Boolean := True;
                            Trim_Names    : Boolean := True;
                            Trim_Comments : Boolean := True;
                            Ignore_Empty  : Boolean := True)
                            return Basic_Line_Classifier
   is
   begin
      if Head = "" and Tail = "" then
         raise Constraint_Error;
      end if;

      return (Head          => To_U (Head),
              Tail          => To_U (Tail),
              Comments      => To_U (Comments),
              Trim_Lines    => Trim_Lines,
              Trim_Names    => Trim_Names,
              Trim_Comments => Trim_Comments,
              Ignore_Empty  => Ignore_Empty);
   end New_Classifier;

   overriding
   function Classify (Parser : Basic_Line_Classifier;
                      Input  : String)
                      return Classified_Line
   is
      use Ada.Strings;

      function Begin_With (Input : String;
                           Head  : Unbounded_String)
                           return Boolean
      is
         H : constant String := To_S (Head);
      begin
         return
           H = "" or else
           (Input'Length >= H'Length
            and then Input (Input'First .. Input'First + H'Length - 1) = H);
      end Begin_With;

      function End_With (Input : String;
                         Tail  : Unbounded_String;
                         Skip  : Natural)
                         return Boolean
      is
         T : constant String := To_S (Tail);
      begin
         return T = ""
           or else
             ((Input'Length - Skip) > T'Length
              and then Input (Input'Last - T'Length + 1 .. Input'Last) = T);
      end End_With;

      function Strip (Input : String;
                      Head  : Unbounded_String;
                      Tail  : Unbounded_String;
                      Trim  : Boolean)
                      return Unbounded_String
      is
         Section : constant String :=
                     Input (Input'First + Length (Head) .. Input'Last - Length (Tail));
      begin
         if Trim then
            return To_U (Fixed.Trim (Section, Both));
         else
            return To_U (Section);
         end if;
      end Strip;

      function Is_To_Be_Ignored (Input : String) return Boolean is
         Trimmed : constant String := Fixed.Trim (Input, Both);
      begin
         if Length (Parser.Comments) > 0 then
            if Begin_With (Input, Parser.Comments) then
               return True;
            end if;

            if Parser.Trim_Comments
              and then Begin_With (Trimmed, Parser.Comments) then
               return True;
            end if;
         end if;

         if Parser.Ignore_Empty and Trimmed = "" then
            return True;
         end if;

         return False;
      end Is_To_Be_Ignored;
   begin
      if Is_To_Be_Ignored (Input) then
         return Classified_Line'(Class => Comment);
      end if;

      if Begin_With (Input, Parser.Head)
        and then End_With (Input, Parser.Tail, Length (Parser.Head)) then
         return Classified_Line'(Class => Section,
                                 Name  => Strip (Input, Parser.Head, Parser.Tail, Parser.Trim_Names));
      end if;

      if Parser.Trim_Lines then
         return Classified_Line'(Class => Normal,
                                 Content => To_U (Fixed.Trim (Input, Both)));
      else
         return Classified_Line'(Class => Normal,
                                 Content => To_U (Input));
      end if;

   end Classify;

end Sectioned_Text;

--        function Check (X : String) return String is
--           use Ada.Strings.Fixed;
--
--        begin
--           if X'Length >= Separator'Length and then
--              Head (X, Separator'Length) = Separator then
--
--              return Tail (X, X'Length - Separator'Length);
--           else
--              return "";
--           end if;
--        end Check;

--        Text_Io.Open (File => Input,
--                      Mode => Text_Io.In_File,
--                      Name => Filename);
--
--        while not Text_Io.End_Of_File (Input) loop
--           Lines.Append (Text_Io.Get_Line (Input));
--        end loop;
--
--        Text_Io.Close (Input);
