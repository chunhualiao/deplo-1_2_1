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

with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Package_Names;
with Style_Table;

package Command_Line_Parameters is
   use Ada.Strings.Unbounded;

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   type Program_Parameters is
      record
         Input_Name    : String_Lists.List;
         Style_Spec    : Style_Table.Style_Spec_Lists.List;
         Input_Format  : Unbounded_String;
         Output_Format : Unbounded_String;
         Trim_Nodes    : Package_Names.Package_List;
         Ignore        : Package_Names.Package_List;
      end record;

   function Parse_Command_Line return Program_Parameters;

   User_Error : exception;
end Command_Line_Parameters;
