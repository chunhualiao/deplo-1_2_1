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

with Package_Trees;
--  with Ada.Text_IO;
with Dependency_Graphs;
with Ada.Strings.Fixed;
with Indented_IO;
with Ada.Strings.Equal_Case_Insensitive;

package body Output is

   use type Style_Table.Style_Spec;

   Default_Style : constant Style_Table.Style_Spec :=  "dot.subgraph-color=red;"
      & "dot.subgraph-penwidth=3.0;"
      & "dot.collapsed-penwidth = 2.0;"
      & "dot.collapsed-shape = folder;"
      & "dot.collapsed-color = blue";

   function To_S (Idx : Package_Trees.Package_Index) return String is
      use Package_Trees;
      use Ada.Strings.Fixed;
      use Ada.Strings;
   begin
      return Trim (Package_Index'Image (Idx), Both);
   end To_S;

   function Node_Name (Pos : Package_Trees.Cursor) return String is
   begin
      return "node_" & To_S (Package_Trees.Index (Pos));
   end Node_Name;

   function Cluster_Name (Pos : Package_Trees.Cursor) return String is
   begin
      return "cluster_" & To_S (Package_Trees.Index (Pos));
   end Cluster_Name;

   procedure Print_Style (Indenter   : in out Indented_IO.Indenter_Type;
                          Class      : Style_Table.Node_Class;
                          Head       : String;
                          Separator  : String;
                          Terminator : String;
                          Multiline  : Boolean) is
      First_Time : Boolean := True;

      procedure Print_Entry (Name  : Style_Table.Attribute_Name;
                             Value : Style_Table.Attribute_Value)
      is
      begin
         if First_Time then
            Indenter.Put (Head);
            First_Time := False;
         else
            Indenter.Put (Separator);
         end if;

         declare
            Item : constant String := String (Name)
               & "="
               & '"' & String (Value) & '"'
               & Terminator;
         begin
            if Multiline then
               Indenter.Put_Line (Item);
            else
               Indenter.Put (Item);
            end if;
         end;
      end Print_Entry;

      use type Style_Table.Node_Class;
   begin
      Style_Table.Iterate ("dot." & Class, Print_Entry'Access);

   end Print_Style;

   procedure Print_Node_List (Indenter : in out Indented_IO.Indenter_Type;
                              DB       : Package_Trees.Package_Tree) is
      use Package_Trees;

      procedure Print_Tree (Pos : Cursor) is
         use Ada.Strings.Fixed;

         procedure Print_Node (Pos : Cursor) is
         begin
            Indenter.Put (Node_Name (Pos));
            Indenter.Put ("[label="
                          & '"' & String (Name (Pos)) & '"');

            if Has_Attribute (Pos, "collapsed") then
               Print_Style (Indenter   => Indenter,
                            Class      => "collapsed",
                            Head       => ",",
                            Separator  => ",",
                            Terminator => "",
                            Multiline  => False);
            end if;

            Indenter.Put_Line ("];");
         end Print_Node;

         procedure Open_Subgraph (Pos : Cursor) is
         begin
            Indenter.Open_Block ("subgraph " & Cluster_Name (Pos)  & "{");

            Indenter.Put_Line ("label=""" & String (Name (Pos)) & """;");

            Print_Style (Indenter   => Indenter,
                         Class      => "subgraph",
                         Head       => "",
                         Separator  => "",
                         Terminator => ";",
                         Multiline  => True);

            --              Indenter.Put_Line ("color=red;");
            --
            --              Indenter.Put_Line ("penwidth=3.0;");

         end Open_Subgraph;

         procedure Close_Subgraph (Pos : Cursor) is
            pragma Unreferenced (Pos);
         begin
            Indenter.Close_Block ("}");
         end Close_Subgraph;

         Desc : constant Cursor_Array := Children (Pos);
      begin
         if Desc'Length = 0 then
            pragma Assert (Is_A_Leaf (Pos));
            Print_Node (Pos);
         else
            pragma Assert (not Is_A_Leaf (Pos));
            Open_Subgraph (Pos);

            for I in Desc'Range loop
               Print_Tree (Desc (I));
            end loop;

            Close_Subgraph (Pos);
         end if;
      end Print_Tree;

      Roots : constant Cursor_Array := DB.Roots;
   begin
      for I in Roots'Range loop
         Print_Tree (Roots (I));
      end loop;
   end Print_Node_List;

   procedure Print_Edges (Indenter : in out Indented_IO.Indenter_Type;
                          Graph    : Dependency_Graphs.Graph_Access;
                          DB       : Package_Trees.Package_Tree) is
      use Package_Trees;

      procedure Print_Single_Edge (From : Package_Trees.Cursor;
                                   To   : Package_Trees.Cursor) is
         --  Print the description in DOT of the edge connecting the
         --  two specified packages.

         function Find_Leaf (Root : Cursor) return Cursor;
         pragma Postcondition (Is_A_Leaf (Find_Leaf'Result));
         --  If Root is a leaf, return root; otherwise, return a
         --  leaf node that is a descendent of Root.  If some child
         --  of Root is a leaf, the result is granted to be a child
         --  of Root.

         function Find_Leaf (Root : Cursor) return Cursor is

         begin
            if Is_A_Leaf (Root) then
               --  No children.  This is a leaf
               return Root;
            else
               declare
                  Kids : constant Cursor_Array := Children (Root);
               begin
                  --  First search among the direct descendants of Root
                  for I in Kids'Range loop
                     if Is_A_Leaf (Kids (I)) then
                        return Kids (I);
                     end if;
                  end loop;

                  --  No direct descendant is a leaf.
                  return Find_Leaf (Kids (Kids'First));
               end;
            end if;
         end Find_Leaf;

         True_From : Cursor;
         True_To   : Cursor;
      begin
         --  The case where From or To has children must be handled
         --  in a special way.  In order to make an edge between a cluster
         --  (corresponding to a package with children) and a node,
         --  we must specify as target (or source) a leaf package
         --  and then specify the cluster as "logical head" (or "logical
         --  tail").  Our first step is to check if From (or To) has
         --  children and, in case it has, to find a leaf among the
         --  its descendants.
         --

         True_From := Find_Leaf (From);
         True_To := Find_Leaf (To);

         pragma Assert ((True_From = From) = Is_A_Leaf (From));
         pragma Assert ((True_To = To) = Is_A_Leaf (To));
         pragma Assert (Is_A_Leaf (True_To));
         pragma Assert (Is_A_Leaf (True_From));

         --  Make an edge between the two leaves
         Indenter.Put (Node_Name (True_From)
                       & " -> "
                       & Node_Name (True_To));

         if True_From /= From or True_To /= To then
            --  If I am here, at least one of the nodes has children.
            --  We must add attribute(s) lhead and/or ltail to the node.

            Indenter.Put (" [");
            --              Put_Line (Standard_Error,
            --                        "to = " & Node_Name (To) &
            --                        " true to = " & Node_Name (True_To) &
            --                        " from = " & Node_Name (From) &
            --                        " true from = " & Node_Name (From));
            if True_From /= From then
               Indenter.Put ("ltail=" & Cluster_Name (From));
            end if;

            if True_From /= From and True_To /= To then
               Indenter.Put (", ");
            end if;

            if True_To /= To then
               Indenter.Put ("lhead=" & Cluster_Name (To));
            end if;

            Indenter.Put ("]");
         end if;

         Indenter.Put_Line (";");
      end Print_Single_Edge;
   begin
      for Idx in DB.First_Index .. DB.Last_Index loop
         Graph.Iterate_Over_Withed (Wither  => DB.Position (Idx),
                                    Process => Print_Single_Edge'Access);
      end loop;
   end Print_Edges;

   procedure Print_Header (Indenter : in out Indented_IO.Indenter_Type) is
   begin
      Indenter.Open_Block ("digraph G {");
      Indenter.Put_Line ("compound=true;");
      Print_Style (Indenter   => Indenter,
                   Class      => "graph",
                   Head       => "",
                   Separator  => "",
                   Terminator => ";",
                   Multiline  => True);

      Indenter.Put ("node [");
      Print_Style (Indenter   => Indenter,
                   Class      => "default_node",
                   Head       => "",
                   Separator  => ",",
                   Terminator => "",
                   Multiline  => False);
      Indenter.Put_Line ("]");
   end Print_Header;

   procedure Print_Trailer (Indenter : in out Indented_IO.Indenter_Type) is
   begin
      Indenter.Close_Block ("}");
   end Print_Trailer;

   -----------------
   -- Print_Graph --
   -----------------

   procedure Print_Graph (Format : String;
                          Item   : Package_DB.DB_Type)
   is
      pragma Unreferenced (Format);
      Indenter : Indented_IO.Indenter_Type := Indented_IO.Create;

      Collapsed : constant Package_DB.DB_Type := Item;
   begin
      Print_Header (Indenter);
      Print_Node_List (Indenter, Collapsed.Tree);
      Print_Edges (Indenter, Collapsed.Graph, Collapsed.Tree);
      Print_Trailer (Indenter);
   end Print_Graph;

   --------------
   -- Is_Known --
   --------------

   function Is_Known (Format : String) return Boolean is
   begin
      return Ada.Strings.Equal_Case_Insensitive (Format, "dot");
   end Is_Known;

   --------------------
   -- Style_Defaults --
   --------------------

   function Style_Defaults (Format : String) return Style_Table.Style_Spec
   is
      pragma Unreferenced (Format);
   begin
      return Default_Style;
   end Style_Defaults;

end Output;
