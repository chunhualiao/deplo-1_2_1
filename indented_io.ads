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
--
--  This package provides a type Indenter that is handy for printing
--  indented text (e.g., HTML, XML, source code...).
--
--  The type provides few procedures with the same name of Text_IO
--  procedures (Put, Put_Line, New_Line) that can be used to emit
--  text.  An Indenter keep track of the current indentation level
--  and add the required amount of spaces at the beginning of
--  every new line.
--
--  Tabbing can be modified both esplicitely (by using the procedures
--  Increase_Tab and Decrease_Tab) or implicitly (by using the procedures
--  Open_Block and Close_Block)
--
with Ada.Text_IO;

package Indented_IO is
   use Ada.Text_IO;

   type Indenter_Type (<>) is tagged private;

   function Create (File : File_Access := Ada.Text_IO.Standard_Output;
                    Tab  : Positive := 4)
                    return Indenter_Type;
   --  Create a new indenter that will print to the specified file.
   --  Tab is the amount of indentation added for each block

   procedure Set_Tab_Size (Indenter : in out Indenter_Type;
                           Tab      : Positive);
   --  Change the amount of indentation added for each block

   procedure Put (Indenter : in out Indenter_Type;
                  Item     : String);
   --  Print the specified string to the Indenter output.  If at the
   --  beginning of a line, add the required amount of spaces

   procedure New_Line (Indenter : in out Indenter_Type);
   --  Add a new line to the indenter output

   procedure Put_Line (Indenter : in out Indenter_Type;
                       Item     : String);
   --  Equivalent to Put followed by New_Line

   procedure Increase_Tab (Indenter : in out Indenter_Type);
   --  Increase the indentation level by the Tab amount

   procedure Decrease_Tab (Indenter : in out Indenter_Type);
   --  Decrease the indentation level by the Tab amount

   procedure Open_Block (Indenter : in out Indenter_Type;
                         Line     : in     String := "");
   --  Close the current line (if not at the beginning of the line),
   --  print the specified string, increase the indentation level
   --  by the Tab amount and do a New_Line

   procedure Close_Block (Indenter : in out Indenter_Type;
                          Line     : in     String := "");
   --  Close the current line (if not at the beginning of the line),
   --  decrease the indentation level by the Tab amount,
   --  print the specified string and do a New_Line.
   --
   --  Note that the order of actions "print" and "change indentation"
   --  are reversed in Open_Block and Close_Block.  This causes the following
   --  code
   --
   --         Indenter.Open_Block ("<div>");
   --         Indenter.Put_Line ("Foo");
   --         Indenter.Close_Block ("</div>");
   --
   --   to produce the following output (supposing Tab=4)
   --
   --       <div>
   --           Foo
   --       </div>

private
   type Indenter_Type is tagged
      record
         File           : Ada.Text_IO.File_Access;
         Current_Indent : Natural := 0;
         Indent_Step    : Natural := 4;
         Begin_Of_Line  : Boolean := True;
      end record;
end Indented_IO;
