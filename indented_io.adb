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
with Ada.Strings.Fixed;

package body Indented_IO is

   ------------
   -- Create --
   ------------

   function Create
     (File : File_Access := Ada.Text_IO.Standard_Output;
      Tab  : Positive := 4)
      return Indenter_Type
   is
   begin
      return Indenter_Type'(File           => File,
                            Current_Indent => 0,
                            Indent_Step    => Tab,
                            Begin_Of_Line  => True);
   end Create;

   ------------------
   -- Set_Tab_Size --
   ------------------

   procedure Set_Tab_Size
     (Indenter : in out Indenter_Type;
      Tab      : Positive)
   is
   begin
      Indenter.Indent_Step := Tab;
   end Set_Tab_Size;

   ---------
   -- Put --
   ---------

   procedure Put
     (Indenter : in out Indenter_Type;
      Item     : String)
   is
      use Ada.Strings.Fixed;
   begin
      if Indenter.Begin_Of_Line then
         Put (Indenter.File.all, Indenter.Current_Indent * ' ');
         Indenter.Begin_Of_Line := False;
      end if;

      Put (Indenter.File.all, Item);
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Indenter : in out Indenter_Type;
      Item     : String)
   is
   begin
      Put (Indenter, Item);
      New_Line (Indenter);
   end Put_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Indenter : in out Indenter_Type) is
   begin
      New_Line (Indenter.File.all);
      Indenter.Begin_Of_Line := True;
   end New_Line;

   ----------------
   -- Open_Block --
   ----------------

   procedure Open_Block
     (Indenter : in out Indenter_Type;
      Line     : in     String := "")
   is
   begin
      if not Indenter.Begin_Of_Line then
         Indenter.New_Line;
      end if;

      if Line /= "" then
         Put_Line (Indenter, Line);
      end if;

      Increase_Tab (Indenter);
   end Open_Block;

   -----------------
   -- Close_Block --
   -----------------

   procedure Close_Block
     (Indenter : in out Indenter_Type;
      Line     : in     String := "")
   is
   begin
      if not Indenter.Begin_Of_Line then
         Indenter.New_Line;
      end if;

      Indenter.Decrease_Tab;

      if Line /= "" then
         Indenter.Put_Line (Line);
      end if;
   end Close_Block;

   ------------------
   -- Increase_Tab --
   ------------------

   procedure Increase_Tab (Indenter : in out Indenter_Type) is
   begin
      Indenter.Current_Indent :=
        Indenter.Current_Indent + Indenter.Indent_Step;
   end Increase_Tab;

   ------------------
   -- DeCrease_Tab --
   ------------------

   procedure Decrease_Tab (Indenter : in out Indenter_Type) is
   begin
      Indenter.Current_Indent :=
        Integer'Max (0, Indenter.Current_Indent - Indenter.Indent_Step);
   end Decrease_Tab;

end Indented_IO;
