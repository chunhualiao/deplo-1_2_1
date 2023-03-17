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
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package body Line_Arrays is
   use Ada.Characters;

   -----------
   -- Split --
   -----------

   function Split
     (Input      : String;
      Terminator : Line_Terminator := Any)
      return Line_Array
   is
      procedure Check_EOL (Cursor    : Positive;
                           Is_At_EOL : out Boolean;
                           End_EOL   : out Positive);
      pragma Precondition (Cursor <= Input'Last);
      pragma Postcondition ((not Is_At_EOL) or else (End_EOL <= Input'Last));

      --  Check if Cursor points to the beginning of a line terminator
      --  If it does, set Is_At_EOL equal to true and set End_EOL to the
      --  position of the last char of the line terminator (that is
      --  equal to Cursor unless the line terminator is a CRLF)

      procedure Check_EOL (Cursor    : Positive;
                           Is_At_EOL : out Boolean;
                           End_EOL   : out Positive)
      is
         EOL_Type : Line_Terminator;
      begin
         case Input (Cursor) is
            when Latin_1.CR =>
               if Cursor < Input'Last and then Input (Cursor + 1) = Latin_1.LF then
                  EOL_Type := CRLF;
               else
                  EOL_Type := CR;
               end if;
            when Latin_1.LF =>
               EOL_Type := LF;
            when others =>
               Is_At_EOL := False;
               return;
         end case;

         if Terminator = Any or Terminator = EOL_Type then
            Is_At_EOL := True;
            if EOL_Type = CRLF then
               End_EOL := Cursor + 1;
            else
               End_EOL := Cursor;
            end if;
         else
            Is_At_EOL := False;
         end if;
      end Check_EOL;

      Result       : Line_Array;
      Cursor       : Positive := Input'First;
      Previous_End : Natural := Input'First - 1;
      End_EOL      : Positive;
      Is_At_EOL    : Boolean;
   begin
      while Cursor <= Input'Last loop
         Check_EOL (Cursor, Is_At_EOL, End_EOL);

         if Is_At_EOL then
            Result.Append (Input (Previous_End + 1 .. Cursor - 1));

            Cursor := End_EOL;
            Previous_End := Cursor;
         end if;

         Cursor := Cursor + 1;
      end loop;

      if Previous_End < Input'Last then
         Result.Append (Input (Previous_End + 1 .. Input'Last));
      end if;

      return Result;
   end Split;

   function Join
     (Input      : Line_Array;
      Terminator : String)
      return String
   is
      use Line_Containers;

      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for I in Input.First_Index .. Input.Last_Index loop
         Result := Result & Input.Element (I);

         if I < Input.Last_Index then
            Result := Result & Terminator;
         end if;
      end loop;

      return To_String (Result);
   end Join;

   ----------
   -- Join --
   ----------

   function Join
     (Input      : Line_Array;
      Terminator : Valid_Line_Terminator := CR)
      return String
   is
   begin
      case Terminator is
         when LF =>
            return Join (Input, "" & Latin_1.CR);
         when CR =>
            return Join (Input, "" & Latin_1.LF);
         when CRLF =>
            return Join (Input, Latin_1.CR & Latin_1.LF);
      end case;
   end Join;

   function "=" (Left, Right : Line_Array) return Boolean
   is
      use Ada.Containers;

      Diff : constant Integer := Right.First_Index - Left.First_Index;
   begin
      if Left.Length /= Right.Length then
         return False;
      end if;

      for I in Left.First_Index .. Left.Last_Index loop
         if Left.Element (I) /= Right.Element (I + Diff) then
            return False;
         end if;
      end loop;

      return True;
   end "=";

   function Read
      (Input      : Character_IO.File_Type;
      Terminator : Line_Terminator := Any)
      return Line_Array
   is
      use Character_IO;

      Tmp : Unbounded_String;
      Ch : Character := 'x';
   begin
      while not End_Of_File (Input) loop
         Read (Input, Ch);
         Tmp := Tmp & Ch;
      end loop;

      return Split (To_String (Tmp), Terminator);
   end Read;

   function Read
     (Filename   : String;
      Terminator : Line_Terminator := Any)
      return Line_Array
   is
      use Ada;

      Input : Character_IO.File_Type;
      Result : Line_Array;
   begin
      Character_IO.Open (File => Input,
                         Mode => Character_IO.In_File,
                         Name => Filename);

      Result := Read (Input, Terminator);

      Character_IO.Close (Input);

      return Result;
   end Read;

end Line_Arrays;
