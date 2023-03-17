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
--  This package defines a record that holds together the tree of
--  packages and their dependency graph.   Maybe a more elegant solution
--  was to define a single "opaque" type that collected together the
--  functionalities of the package tree and dependency graph, without
--  showing the two parts as separate component of a record.  The reason
--  for this solution is mainly hystorical and, honestly, the code
--  is so simple that I decided it was not worth to reorganize it.
--
with Dependency_Graphs;
with Package_Trees;

package Package_DB is
   use Dependency_Graphs;
   use Package_Trees;

   type DB_Type is
      record
         Tree  : Package_Tree;
         Graph : Graph_Access;
      end record;
end Package_DB;
