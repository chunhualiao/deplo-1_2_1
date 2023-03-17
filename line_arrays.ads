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

--
--  This package provides method for working with "arrays of line."
--  Nothing georgious, really, just a place where few handy routines
--  are kept.
--
with Ada.Containers.Indefinite_Vectors;
with Ada.Sequential_IO;

package Line_Arrays is
   package Character_IO is
      new Ada.Sequential_IO (Character);

   package Line_Containers is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                            Element_Type => String);

   subtype Line_Array is Line_Containers.Vector;

   type Line_Terminator is  -- Which caracters sequence ends the line?
      (
       CR,      --  Only Carriage Return ends the line
       LF,      --  Only Line Feed ends the line
       CRLF,    --  Only the sequence CR + LF ends the line
       Any      --  Any of the above combination is admitted
      );

   subtype Valid_Line_Terminator is Line_Terminator range CR .. CRLF;

   function Split
     (Input      : String;
      Terminator : Line_Terminator := Any)
      return Line_Array;
   --  Split a string into an array of lines, splitting at the specified
   --  line terminator

   function Read
     (Filename   : String;
      Terminator : Line_Terminator := Any)
      return Line_Array;
   --  Read a file and split its content a string into an array of lines,
   --  splitting at the specified line terminator

   function Read
     (Input      : Character_IO.File_Type;
      Terminator : Line_Terminator := Any)
      return Line_Array;
   --  Read a file and split its content a string into an array of lines,
   --  splitting at the specified line terminator

   function Join
     (Input      : Line_Array;
      Terminator : Valid_Line_Terminator := CR)
      return String;
   --  Inverse of split: take an array of line and join them together
   --  with the specified terminator

   function Join
     (Input      : Line_Array;
      Terminator : String)
      return String;
   --  Generalization of the other Join; in this case the terminator
   --  can be any string.  Quite similar to the join in many script languages.

   function "=" (Left, Right : Line_Array) return Boolean;

end Line_Arrays;
