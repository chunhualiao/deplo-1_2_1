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

with Package_DB;
with Input;
with Output;

with Command_Line_Parameters;
with Style_Table;
with Node_Processing;

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Exceptions;

procedure Deplo is
   use Ada.Text_IO;
   use Ada.Command_Line;
   use Ada.Exceptions;

   use Node_Processing;

   function "+" (X : Ada.Strings.Unbounded.Unbounded_String)
                  return String
                  renames Ada.Strings.Unbounded.To_String;

   Parameters   : Command_Line_Parameters.Program_Parameters;

   Structure    : Package_DB.DB_Type;
begin
   Parameters := Command_Line_Parameters.Parse_Command_Line;

   declare
      Input_Format  : constant String := +Parameters.Input_Format;
      Output_Format : constant String := +Parameters.Output_Format;
   begin
      if not Input.Is_Known (Input_Format) then
         raise Command_Line_Parameters.User_Error
         with "Unknown input format '" & Input_Format & "'";
      end if;

      if not Output.Is_Known (Output_Format) then
         raise Command_Line_Parameters.User_Error
         with "Unknown output format '" & Output_Format & "'";
      end if;

      --  Style_Table.Dump(Parameters.Style_Spec);

      Style_Table.Parse_And_Add (Output.Style_Defaults (Output_Format));
      Style_Table.Parse_And_Add (Parameters.Style_Spec);

      Structure := Input.Get_Package_Structure (Format      => Input_Format,
                                                Source_Name => Parameters.Input_Name);

      Structure := Collapse_Nodes (Input     => Structure,
                                   Trim_List => Parameters.Trim_Nodes);

      Structure := Remove_Ignored (Input       => Structure,
                                   Ignore_List => Parameters.Ignore);

      Clean_Dependencies (Structure.Graph, Structure.Tree);

      Output.Print_Graph (Output_Format, Structure);

      Set_Exit_Status (Success);
   end;
exception
   when E : Command_Line_Parameters.User_Error  =>
      Put_Line (Standard_Error, "Error: " & Exception_Message (E));
      Set_Exit_Status (Failure);
end Deplo;
