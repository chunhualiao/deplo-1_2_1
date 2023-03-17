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
--  This package contains some procedures used to process the
--  dependency graph.  There is no special software-engineering
--  reason that asks for these procedures to have a package
--  of their own.  It is just to keep the code a bit cleaner.
--

with Package_DB;
with Package_Names;
with Dependency_Graphs;
with Package_Trees;

package Node_Processing is
   Collapsed_Attribute : constant String := "collapsed";

   function Collapse_Nodes (Input      : Package_DB.DB_Type;
                            Trim_List  : Package_Names.Package_List)
                            return Package_DB.DB_Type;
   --  "Collapse" the nodes that are descendants of the nodes of Trim_List
   --  into their ancestor belonging to Trim_List and update the edges
   --  consequently.  For example, if A.B is in Trim_List and A.B.C "with"s
   --  D.E and G.H "with"s A.B.Q, then the final graph will have the
   --  edges
   --            A.B -> D.E (instead of A.B.C -> D.E)
   --            G.H -> A.B (instead of G.H -> A.B.Q)
   --
   --  To the nodes in Trim_List it will be assigned the attribute
   --  Collapsed_Attribute (with value an empty string) that can be used
   --  by the output procedure to recognize collapsed nodes and assign them
   --  a special representation.

   function Remove_Ignored (Input        : Package_DB.DB_Type;
                            Ignore_List  : Package_Names.Package_List)
                            return Package_DB.DB_Type;
   --  Remove the nodes in the Ignore_List and remove the corresponding
   --  edges too.

   procedure Clean_Dependencies (Graph : Dependency_Graphs.Graph_Access;
                                 DB    : Package_Trees.Package_Tree);
   --  Remove redundant dependencies, in particular
   --
   --      * Dependencies from a descendant to an ancestor and
   --        vice-versa (this could remove some genuine dependencies if
   --        a package "with"s a descendant)
   --
   --      * If a package depends both from a package and an ancestor of
   --        it, remove the dependence from the ancestor.

end Node_Processing;
