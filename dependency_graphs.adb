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
with Ada.Text_IO;

package body Dependency_Graphs is

   ------------
   -- Create --
   ------------

   function Create (N_Nodes : Positive) return Graph_Access is

   begin
      return new Dependency_Graph'(N_Nodes   => Package_Index (N_Nodes),
                                   Adjacency => (others => (others => False)));
   end Create;

   --------------------
   -- Add_Dependency --
   --------------------

   procedure Add_Dependency
     (Graph  : in out Dependency_Graph;
      Wither : in     Package_Trees.Cursor;
      Withed : in     Package_Trees.Cursor)
   is
   begin
      Graph.Adjacency (Index (Wither), Index (Withed)) := True;
   end Add_Dependency;

   -----------------------
   -- Remove_Dependency --
   -----------------------

   procedure Remove_Dependency
     (Graph  : in out Dependency_Graph;
      Wither : in     Package_Trees.Cursor;
      Withed : in     Package_Trees.Cursor)
   is
   begin
      Graph.Adjacency (Index (Wither), Index (Withed)) := False;
   end Remove_Dependency;

   -------------------------
   -- Iterate_Over_Withed --
   -------------------------

   procedure Iterate_Over_Withed
     (Graph   : Dependency_Graph;
      Wither  : Package_Trees.Cursor;
      Process : access procedure (Wither : Package_Trees.Cursor;
                                  Withed : Package_Trees.Cursor))
   is
   begin
      for I in Graph.Adjacency'Range (2) loop
         if Graph.Adjacency (Index (Wither), I) then
            Process (Wither, Position (Wither, I));
         end if;
      end loop;
   end Iterate_Over_Withed;

   ----------
   -- Dump --
   ----------

   procedure Dump (Graph   : Dependency_Graph)
   is
      use Ada.Text_IO;
   begin
      for R in Graph.Adjacency'Range (1) loop
         for C in Graph.Adjacency'Range (2) loop
            if Graph.Adjacency (R, C) then
               Put_Line (Standard_Error,
                         Package_Index'Image (R) & "->" & Package_Index'Image (C));
            end if;
         end loop;
      end loop;
   end Dump;

end Dependency_Graphs;
