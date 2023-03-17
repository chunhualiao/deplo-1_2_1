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
--  This package provides a "style table," that is, a structure that
--  keeps track of the style attributes to be used to output the graph.
--
--  This package works as a "singleton object," that is, one does not
--  allocate a "style table" and use it, but uses directly this package.
--  Although this type of design reduce re-usability, it must be said
--  that what this package provides is so specific to DePlo that the
--  possiblity of a reuse is quite remote.
--
--  Graph nodes are partitioned in classes.  The set of classes depends
--  on the output format. Every node class has its set of attributes.
--  For example, for the DOT format  we currently have two classes: the
--  "subgraph" class that applies to DOT subgraph and the "collapsed"
--  class that applies to collapsed subtree.  By default, for example,
--  the collapsed class has the attributes
--
--      color=blue
--      shape=folder
--      penwidth=3.0
--
--  This is why, by default, the collapsed subgraphs are shown as
--  a thick, blue folder.
--
--  The complete name for an attribute has the form
--
--      <format>.<node_class>-<attribute_name>
--
--  For example, the complete name for the color of a subgraph in the
--  DOT output format is
--
--         dot.subgraph-color
--
--  Actually, currently in the API the whole <format>.<node_class> is
--  considered the actual class name, without distinguishing between the
--  format and node_class parts.  This is an hystorical accident, when
--  there was only one output format and we feel that there is little
--  to gain in separating the two components.
--
with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Style_Table is
   use Ada.Strings.Unbounded;

   type Attribute_Name is new String;

   type Attribute_Value is new String;

   type Node_Class is new String;

   type Style_Spec is new String;

   package Style_Spec_Lists is
      new Ada.Containers.Indefinite_Doubly_Linked_Lists (Style_Spec);

   procedure Dump (List : Style_Spec_Lists.List);

   procedure Add (Class  : Node_Class;
                  Name   : Attribute_Name;
                  Value  : Attribute_Value;
                  Append : Boolean := False);
   --  Add a new attribute for the specified node class.  If the attribute
   --  already exists, its value is overwritten, unless Append is true.

   procedure Extract_Spec_Data (Input  : in     Style_Spec;
                                Cursor : in out Natural;
                                Class  :    out Unbounded_String;
                                Name   :    out Unbounded_String;
                                Value  :    out Unbounded_String;
                                Append :    out Boolean);
   --  Parse a style spec string.  At the first call Cursor must
   --  be initialized to zero. At the end of the procedure Cursor will
   --  hold the index of the next character to be read, so that calling
   --  succesively this function will parse the whole string.  If the
   --  spec is empty, Class is returned equal to the empty string.
   --
   --  The syntax for the spec is as follows
   --
   --      spec_data    = single_spec (';' single_spec)*
   --      single_spec  = attribute ('=' value)?
   --      attribute    = class '-' name
   --      class        = [^-]+
   --      name         = [^=;]+
   --      value        = [^;]+ | value_string
   --      value_string = '"' *([^"] | '""') '"'
   --
   --  Class, name and value are trimmed from spaces at both ends, the
   --  only exception being the value_string.
   --  Examples of valid spec strings are
   --
   --     cluster-color=red
   --     cluster-label="long label ""with quotes"""
   --     default-label="label with spaces at the end  "
   --     compressed-show
   --     default-size=12 ; default-shape = circle
   --

   Parsing_Error : exception;

   procedure Parse_And_Add (Spec : Style_Spec);
   --  Sytactic sugar: call reapetly Extract_Spec_Data and add the
   --  result to the style table

   procedure Parse_And_Add (Specs : Style_Spec_Lists.List);
   --  Call Extract_Spec_Data for every entry in the list of
   --  style specs and add the result to the style table

   procedure Iterate
      (Class : Node_Class;
       Process : not null access procedure (Name : Attribute_Name;
                                            Value : Attribute_Value));
   --  Call the Process procedure for every attribute associated
   --  with the specified class.

end Style_Table;
