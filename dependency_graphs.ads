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
--  The dep2dot program keeps the informations about the packages in
--  two structures: a tree of packages (implemented in package_trees),
--  describing the "parent-child" relationship between packages and a
--  dependency graph (implemented in Dependency_Graphs) describing
--  who "with"s whom.
--
--  This package defines a type representing such a "dependency graph."
--  The two extremes of an edge are called "wither" (the package that
--  "with"s) and "withed."  I know that those are not real English
--  words, but I think that this convention is clear enough and it
--  avoids misunderstanding (e.g. "the head of the edge which package
--  represents?"...)
--
with Package_Trees;

package Dependency_Graphs is
   type Dependency_Graph (<>) is tagged private;

   type Graph_Access is access Dependency_Graph;

   function Create (N_Nodes : Positive) return Graph_Access;

   procedure Add_Dependency (Graph  : in out Dependency_Graph;
                             Wither : in     Package_Trees.Cursor;
                             Withed : in     Package_Trees.Cursor);

   procedure Remove_Dependency (Graph  : in out Dependency_Graph;
                                Wither : in     Package_Trees.Cursor;
                                Withed : in     Package_Trees.Cursor);

   procedure Iterate_Over_Withed
      (Graph   : Dependency_Graph;
       Wither  : Package_Trees.Cursor;
       Process : access procedure (Wither : Package_Trees.Cursor;
                                   Withed : Package_Trees.Cursor));

   procedure Dump (Graph   : Dependency_Graph);
   --  Print on Standard Error a representation of the graph.  Useful
   --  for debug

private
   use Package_Trees;

   type Adjacency_Matrix is
     array (Package_Index range <>, Package_Index range <>) of Boolean;

   type Dependency_Graph (N_Nodes : Package_Index) is tagged
      record
         Adjacency : Adjacency_Matrix (1 .. N_Nodes, 1 .. N_Nodes);
      end record;
end Dependency_Graphs;
