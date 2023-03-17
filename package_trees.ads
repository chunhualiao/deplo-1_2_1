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
--  DePlo keeps the informations about the packages in two structures:
--  a tree of packages (implemented in package_trees), describing
--  the "parent-child" relationship between packages and a
--  dependency graph (implemented in Dependency_Graphs) describing
--  who "with"s whom.
--
--  This package defines a structure used to represent the package tree.
--  The interface is somehow inspired to the ada containers and it defines
--  a "Cursor" type that can be used to access and navigate the structure.
--  A special characteristic of this structure is that to each package
--  is assigned a unique index, represented by a positive number.  Indexes
--  are automatically assigned, in increasing order, starting from 1.  This
--  makes it easier to enumerate the nodes and it is used also to name
--  the nodes in the generated "dot" file.
--
--  Each package can also have a set of "attributes,"  where each
--  attribute is a string pair (name, value).
--
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;
with Ada.Finalization;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Equal_Case_Insensitive;
with Package_Names;

package Package_Trees is
   use Package_Names;

   type Package_Index is new Positive;
   --  To each package the tree automatically assigns a unique index
   --  of this type.

   type Package_Tree is  new
      Ada.Finalization.Controlled
   with
      private;
   --  The type representing the package tree

   type Cursor is private;

   No_Element : constant Cursor;

   type Cursor_Array is array (Positive range <>) of Cursor;

   procedure Add (DB   : in out Package_Tree;
                  Item : in     Package_Name);
   --  Add a new package to the tree

   procedure Set_Attribute (Pos   : Cursor;
                            Name  : String;
                            Value : String := "");
   --  Set an attribute for the node associated with Pos

   function Has_Attribute (Pos   : Cursor;
                           Name : String)
                           return Boolean;
   --  Return true if the attribute with the given name was set

   function Get_Attribute (Pos   : Cursor;
                           Name : String)
                           return String;
   --  Return the value associated with the attribute.  Raise
   --  Constraint_Error if Has_Attribute(Pos, Name) is false

   function Get_Attribute (Pos   : Cursor;
                           Name    : String;
                           Default : String)
                           return String;
   --  Return the value associated with the attribute or the default
   --  value if the attribute is not set

   procedure Attribute_Iterate (Position : Cursor;
                                Process  : access procedure (Name, Value : String));
   --  Call Process for every attribute of the node

   function Find (DB   : Package_Tree;
                  Name : Package_Name)
                  return Cursor;
   --  Return the cursor relative to the package with the given name
   --  If the package is not present, return No_Element

   function Contains (DB   : Package_Tree;
                      Name : Package_Name)
                         return Boolean;
   --  Equivalent to Find(DB, Name) /= No_Element

   function Parent (Position : Cursor)
                    return Cursor;
   --  Return the parent of the node identified by the cursor

   function Children (Pos : Cursor)
                      return Cursor_Array;
   --  Return the cursors associated with the children of the specified node

   function Is_A_Leaf (Pos : Cursor)
                       return Boolean;
   --  Return true if the node has no children.  Equivalent to
   --  Children(pos)'length = 0

   function Roots (DB : Package_Tree)
                   return Cursor_Array;
   --  Return the cursors of the nodes that have no ancestor.  (Yes, the
   --  "package tree" is actually a forest)

   function Index (Position : Cursor)
                   return Package_Index;
   --  Return the unique index assigned to the package identified by the cursor

   function Name  (Position : Cursor)
                   return Package_Name;
   --  Return the name of the package identified by the cursor

   function Position (DB    : Package_Tree;
                      Index : Package_Index)
                      return Cursor;
   pragma Precondition (DB.Last_Index >= Index);

   --  Return the cursor associated with the specified package index in
   --  the specified tree

   function Position (Pos : Cursor;
                      Index : Package_Index)
                      return Cursor;
   --  Return the cursor associated with the specified package index in
   --  the same tree of the specified cursor. (Funny as it may seem, it is
   --  useful)

   function Size (DB : Package_Tree) return Natural;
   --  Return the number of nodes in the tree

   function First_Index (DB : Package_Tree) return Package_Index;
   pragma Precondition (DB.Size > 0);

   --  Return the lowest assigned index.  Always equal to Package_Index'First

   function Last_Index (DB : Package_Tree) return Package_Index;
   pragma Precondition (DB.Size > 0);
   --  Return the highest assigned index.

private
   use Ada.Strings.Unbounded;

   subtype Valid_Package_Index is
     Package_Index range Package_Index'First .. Package_Index'Last - 1;

   No_Index : constant Package_Index := Package_Index'Last;

   function Name_List_Hash (Item : Component_List)
                            return Ada.Containers.Hash_Type;

   function "=" (Left, Right : Component_List)
                 return Boolean;

   package Name_To_Index_Maps is
     new Ada.Containers.Hashed_Maps (Key_Type        => Component_List,
                                     Element_Type    => Package_Index,
                                     Hash            => Name_List_Hash,
                                     Equivalent_Keys => "=");

   package Index_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Package_Index);

   package Attribute_Lists is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => String,
        Hash            => Ada.Strings.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive,
        "="             => Ada.Strings.Equal_Case_Insensitive);

   type Package_Descriptor is
      record
         Index           : Package_Index;
         Parent          : Package_Index;
         Name            : Unbounded_String;
         Name_Components : Component_List;
         Children        : Index_Lists.List;
         Attributes      : Attribute_Lists.Map;
      end record;

   package Package_Vectors is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Package_Index,
                                            Element_Type => Package_Descriptor);

   type Basic_DB_Type is tagged limited
      record
         Name_To_Index : Name_To_Index_Maps.Map;
         Package_Data  : Package_Vectors.Vector;
         Root_Nodes    : Index_Lists.List;
      end record;

   type DB_Access is access Basic_DB_Type;

   type Package_Tree is new
     Ada.Finalization.Controlled
   with
      record
         DB : DB_Access;
      end record;

   type Cursor is
      record
         DB    : DB_Access;
         Index : Package_Index;
      end record;

   No_Element : constant Cursor := (DB => null, Index => No_Index);

   overriding
   procedure Initialize (Obj : in out Package_Tree);
end Package_Trees;
