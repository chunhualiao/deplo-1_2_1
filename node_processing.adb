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

package body Node_Processing is
   ------------------------
   -- Clean_Dependencies --
   ------------------------

   procedure Clean_Dependencies (Graph : in  Dependency_Graphs.Graph_Access;
                                 DB    : in     Package_Trees.Package_Tree)
   is
      use type Package_Trees.Cursor;

      procedure Remove_Direct_Line_Dependencies (Node : Package_Trees.Cursor) is
         Other : Package_Trees.Cursor := Node;
      begin
         while Other /= Package_Trees.No_Element loop

            Graph.Remove_Dependency (Node, Other);
            Graph.Remove_Dependency (Other, Node);

            Other := Package_Trees.Parent (Other);
         end loop;
      end Remove_Direct_Line_Dependencies;

      procedure Remove_Implicit_Dependencies (Wither : Package_Trees.Cursor;
                                              Withed : Package_Trees.Cursor)
      is
         Node : Package_Trees.Cursor := Package_Trees.Parent (Withed);
      begin
         while Node /= Package_Trees.No_Element loop
            Graph.Remove_Dependency (Wither => Wither, Withed => Node);
            Node := Package_Trees.Parent (Node);
         end loop;
      end Remove_Implicit_Dependencies;

      Wither : Package_Trees.Cursor;
   begin
      for Node_Number in DB.First_Index .. DB.Last_Index loop

         Wither := Package_Trees.Position (DB, Node_Number);

         Remove_Direct_Line_Dependencies (Wither);

         Graph.Iterate_Over_Withed (Wither  => Wither,
                                    Process => Remove_Implicit_Dependencies'Access);
      end loop;
   end Clean_Dependencies;

   ---------------------
   -- Removed_Ignored --
   ---------------------

   function Remove_Ignored (Input        : Package_DB.DB_Type;
                             Ignore_List  : Package_Names.Package_List)
                             return Package_DB.DB_Type
   is
      use Package_Trees;
      use Package_Names;
      use Dependency_Graphs;

      procedure Copy_Not_Ignored (Target  : in out Package_Tree;
                                  Source  : in     Package_Tree;
                                  Ignored : in     Package_List)
      is
         Pos : Package_Trees.Cursor;
      begin
         for Index in Source.First_Index .. Source.Last_Index loop
            Pos := Source.Position (Index);

            if Find_Ancestor (Ignored, Name (Pos))  = ""
              and then not Ignored.Contains (Name (Pos))
            then
               Target.Add (Name (Pos));

               declare
                  P : constant Package_Trees.Cursor := Target.Find (Name (Pos));

                  procedure Copy_Attribute (Name, Value : String) is
                  begin
                     Package_Trees.Set_Attribute (Pos   => P,
                                                  Name  => Name,
                                                  Value => Value);
                  end Copy_Attribute;
               begin
                  Package_Trees.Attribute_Iterate (Pos, Copy_Attribute'Access);
               end;
            end if;
         end loop;
      end Copy_Not_Ignored;

      procedure Copy_Valid_Edges (Target  : in out Package_DB.DB_Type;
                                  Source  : in     Package_DB.DB_Type;
                                  Ignored : in     Package_List)
      is
         procedure Process (Wither : Package_Trees.Cursor;
                            Withed : Package_Trees.Cursor) is

            Wither_Pos : constant Cursor := Target.Tree.Find (Name (Wither));
            Withed_Pos : constant Cursor := Target.Tree.Find (Name (Withed));
         begin
            if Wither_Pos /= No_Element and Withed_Pos /= No_Element then
               Target.Graph.Add_Dependency (Wither => Wither_Pos,
                                            Withed => Withed_Pos);
            end if;
         end Process;

         Src : Package_Trees.Cursor;
      begin
         for Index in Source.Tree.First_Index .. Source.Tree.Last_Index loop
            Src := Source.Tree.Position (Index);

            if Target.Tree.Find (Name (Src)) /= Package_Trees.No_Element then
               Source.Graph.Iterate_Over_Withed (Wither  => Src,
                                                 Process => Process'Access);
            end if;
         end loop;
      end Copy_Valid_Edges;

      Result : Package_DB.DB_Type;
   begin
      Copy_Not_Ignored (Target  => Result.Tree,
                        Source  => Input.Tree,
                        Ignored => Ignore_List);

      Result.Graph := Dependency_Graphs.Create (Result.Tree.Size);

      Copy_Valid_Edges (Target  => Result,
                        Source  => Input,
                        Ignored => Ignore_List);

      return Result;
   end Remove_Ignored;

   --------------------
   -- Collapse_Nodes --
   --------------------

   function Collapse_Nodes (Input      : Package_DB.DB_Type;
                            Trim_List  : Package_Names.Package_List)
                            return Package_DB.DB_Type
   is
      use Package_Trees;
      use Package_Names;
      use Dependency_Graphs;

      procedure Copy_Non_Trimmed_Nodes (Target    : in out Package_Tree;
                                        Source    : in     Package_Tree;
                                        Trim_List : in     Package_List)
      is
         Pos    : Package_Trees.Cursor;
      begin
         for Index in Source.First_Index .. Source.Last_Index loop
            Pos := Source.Position (Index);

            if Find_Ancestor (Trim_List, Name (Pos))  = "" then
               Target.Add (Name (Pos));

               if Trim_List.Contains (Name (Pos)) then
                  Set_Attribute (Target.Find (Name (Pos)), Collapsed_Attribute);
               end if;
            end if;
         end loop;
      end Copy_Non_Trimmed_Nodes;

      procedure Collapse_Edges (Target : in out Package_DB.DB_Type;
                                Source : in     Package_DB.DB_Type;
                                Trim_List : in     Package_List)
      is
         procedure Process (Wither : Package_Trees.Cursor;
                            Withed : Package_Trees.Cursor) is

            function Normalize (Node      : Package_Trees.Cursor;
                                Trim_List : Package_List)
                                return Package_Trees.Cursor
            is
               Ancestor : constant Package_Name := Find_Ancestor (Trim_List, Name (Node));
            begin
               if Ancestor = "" then
                  return Target.Tree.Find (Name (Node));
               else
                  return Target.Tree.Find (Ancestor);
               end if;
            end Normalize;

         begin
            Target.Graph.Add_Dependency (Wither => Normalize (Wither, Trim_List),
                                         Withed => Normalize (Withed, Trim_List));
         end Process;
      begin
         for Index in Source.Tree.First_Index .. Source.Tree.Last_Index loop
            Source.Graph.Iterate_Over_Withed (Wither  => Source.Tree.Position (Index),
                                              Process => Process'Access);
         end loop;
      end Collapse_Edges;

      Result : Package_DB.DB_Type;
   begin
      Copy_Non_Trimmed_Nodes (Target    => Result.Tree,
                              Source    => Input.Tree,
                              Trim_List => Trim_List);

      Result.Graph := Dependency_Graphs.Create (Result.Tree.Size);

      Collapse_Edges (Target    => Result,
                      Source    => Input,
                      Trim_List => Trim_List);

      return Result;
   end Collapse_Nodes;

end Node_Processing;
