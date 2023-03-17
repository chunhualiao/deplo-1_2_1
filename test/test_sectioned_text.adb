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

with Sectioned_Text;
with Line_Arrays;
with Ada.Text_IO;

procedure Test_Sectioned_Text is
   use Sectioned_Text, Line_Arrays, Ada.Text_IO;

   procedure Print (X : Section_List) is
      use Sectioned_Text.Line_Array_Maps;

      procedure Print_Section (Pos : Cursor) is
         Lines : constant Line_Array := Element (Pos);
      begin
         Put_Line ("[" & Key (Pos) & "]");

         for I in Lines.First_Index .. Lines.Last_Index loop
            Put_Line (Lines.Element (I) & "//");
         end loop;
      end Print_Section;
   begin
      X.Iterate (Print_Section'Access);
   end Print;

   Sectioned : constant Section_List := Parse ("test_sectioned_text.txt");

   Classifier : constant Basic_Line_Classifier :=
                  New_Classifier (Head          => "[",
                                  Tail          => "]",
                                  Comments      => "%",
                                  Trim_Lines    => True,
                                  Trim_Names    => True,
                                  Trim_Comments => True,
                                  Ignore_Empty  => True);

   Classifier_2 : constant Basic_Line_Classifier :=
                    New_Classifier (Head          => "[",
                                    Tail          => "]",
                                    Comments      => "%",
                                    Trim_Lines    => False,
                                    Trim_Names    => False,
                                    Trim_Comments => False,
                                    Ignore_Empty  => False);

   Sectioned_2 : constant Section_List := Parse (Filename   => "test_sectioned_text2.txt",
                                        Classifier => Classifier);

   Sectioned_3 : constant Section_List := Parse (Filename   => "test_sectioned_text2.txt",
                                        Classifier => Classifier_2);
begin
   Print (Sectioned);
   Put_Line ("-------------------------");
   Print (Sectioned_2);
   Put_Line ("-------------------------");
   Print (Sectioned_3);
end Test_Sectioned_Text;
