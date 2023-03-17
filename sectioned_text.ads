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
--  This package provides procedure to read "sectioned files," that can
--  be used as light-weight configuration files.
--
--  A "sectioned file" is read as a sequence of line.  Every line belongs
--  to one of the following classes
--
--     1. Regular lines
--     2. Section names
--     3. Ignored lines (e.g., comments, but also empty lines)
--
--  The duty of distinguishing between the three classes is assigned
--  a "line classifier," an object descendant of the Abstract_Classifier
--  defined in this package.  This package defines also a
--  Basic_Line_Classifier, derived from Abstract_Classifier, that should
--  cover most of the cases of interest.
--
--  The lines following a section line are considered belonging to that
--  section; the lines between the beginning of the file and the first
--  section line belongs to an "anonymous" section whose name is the empty
--  string.
--
--
--
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded;
with Line_Arrays;

package Sectioned_Text is
   use Line_Arrays;
   use Ada.Strings.Unbounded;

   package Line_Array_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Key_Type     => String,
                                                 Element_Type => Line_Array);

   subtype Section_List is Line_Array_Maps.Map;

   type Line_Class is (Normal, Section, Comment);

   type Classified_Line (Class : Line_Class) is
      record
         case Class is
            when Normal =>
               Content : Unbounded_String;
            when Section =>
               Name    : Unbounded_String;
            when Comment =>
               null;
         end case;
      end record;

   type Abstract_Classifier is interface;

   function Classify (Parser : Abstract_Classifier;
                      Input  : String)
                      return Classified_Line
                      is abstract;

   function Parse
     (Lines       : Line_Array;
      Classifier  : Abstract_Classifier'Class;
      Skip        : Integer := 0)
      return Section_List;
   --  Generalized version of the Parse function.  In this case the
   --  criterion for the

   function Parse
     (Filename    : String;
      Classifier  : Abstract_Classifier'Class;
      Skip        : Integer := 0)
      return Section_List;

   function Parse (Lines : Line_Array) return Section_List;

   function Parse (Filename : String) return Section_List;
   --  Hystorical sectioned text parser, now superseded by the
   --  above functions, but kept for back compatibility and also because
   --  it provides a ready-to-use format.
   --
   --  The first line of the file is a "marker."  Section lines begin
   --  with the marker, comment lines begin with '#', all the other lines are
   --  content line.
   --  For example, the file
   --
   --    %
   --    line 1
   --    line 2
   --    %foo
   --    line 3
   --    line 4
   --    %bar
   --    line 5
   --
   --  has three sections: an anonymous section (lines 1 and 2), section "foo"
   --  (lines 3 and 4) and section "bar."  Spaces at the beginning and end of
   --  the section name are stripped, so that "%foo" is equivalent to "%  foo"
   --  "%foo  ".

   type Basic_Line_Classifier (<>) is new
     Abstract_Classifier
   with
     private;

   function New_Classifier (Head          : String;
                            Tail          : String := "";
                            Comments      : String := "#";
                            Trim_Lines    : Boolean := True;
                            Trim_Names    : Boolean := True;
                            Trim_Comments : Boolean := True;
                            Ignore_Empty  : Boolean := True)
                            return Basic_Line_Classifier;
   overriding
   function Classify (Parser : Basic_Line_Classifier;
                      Input  : String)
                      return Classified_Line;

private

   type Basic_Line_Classifier is new
     Abstract_Classifier
   with
      record
         Head          : Unbounded_String;
         Tail          : Unbounded_String;
         Comments      : Unbounded_String;
         Trim_Comments : Boolean;
         Trim_Lines    : Boolean;
         Trim_Names    : Boolean;
         Ignore_Empty  : Boolean;
      end record;

end Sectioned_Text;
