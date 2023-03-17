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
with Ada.Strings.Hash;

package body Package_Trees is

   ---------
   -- Add --
   ---------

   procedure Add
     (DB   : in out Package_Tree;
      Key  : Component_List)
   is
      procedure Find_Parent (DB     : in out Package_Tree;
                             Key    : Component_List;
                             Parent : out Package_Index)
      is
         Head : Component_List := Key;
      begin
         Head.Delete_Last;
         if not DB.DB.Name_To_Index.Contains (Head) then
            Add (DB, Head);
         end if;

         Parent := DB.DB.Name_To_Index.Element (Head);
      end Find_Parent;

      use type Ada.Containers.Count_Type;

      Parent : Package_Index;
   begin
      if DB.DB.Name_To_Index.Contains (Key) then
         return;
      end if;

      if Key.Length > 1 then
         Find_Parent (DB, Key, Parent);
      else
         Parent := No_Index;
      end if;

      declare
         Idx : constant Package_Index := DB.DB.Package_Data.Last_Index + 1;

         procedure Add_Child (Item : in out Package_Descriptor) is
         begin
            Item.Children.Append (Idx);
         end Add_Child;
      begin
         DB.DB.Name_To_Index.Insert (Key, Idx);

         DB.DB.Package_Data.Append ((Index           => Idx,
                                     Parent          => Parent,
                                     Name            => To_Unbounded_String (String (To_Name (Key))),
                                     Name_Components => Key,
                                     Children        => Index_Lists.Empty_List,
                                     Attributes      => Attribute_Lists.Empty_Map));

         if Parent /= No_Index then
            DB.DB.Package_Data.Update_Element (Index   => Parent,
                                               Process => Add_Child'Access);
         else
            DB.DB.Root_Nodes.Append (Idx);
         end if;
      end;
   end Add;

   procedure Add
     (DB   : in out Package_Tree;
      Item : in     Package_Name)
   is
   begin
      Add (DB, To_Component_List (Item));
   end Add;

   ----------
   -- Find --
   ----------

   function Find
     (DB   : Package_Tree;
      Name : Package_Name)
      return Cursor
   is
      Key : constant Component_List := To_Component_List (Name);
   begin
      if not DB.DB.Name_To_Index.Contains (Key) then
         return No_Element;
      else
         return Cursor'(DB    => DB.DB,
                        Index => DB.DB.Name_To_Index.Element (Key));
      end if;
   end Find;

   ------------
   -- Parent --
   ------------

   function Parent
     (Position : Cursor)
      return Cursor
   is
      Parent_Idx : constant Package_Index := Position.DB.Package_Data.Element (Position.Index).Parent;
   begin
      if Parent_Idx = No_Index then
         return No_Element;
      else
         return (Position.DB, Parent_Idx);
      end if;
   end Parent;

   -----------
   -- Index --
   -----------

   function Index
     (Position : Cursor)
      return Package_Index
   is
   begin
      return Position.Index; -- Position.DB.Package_Data.Element (Position.Index).Index;
   end Index;

   ----------
   -- Name --
   ----------

   function Name
     (Position : Cursor)
      return Package_Name
   is
   begin
      return Package_Name (To_String (Position.DB.Package_Data.Element (Position.Index).Name));
   end Name;

   --------------
   -- Position --
   --------------

   function Position
     (DB    : Package_Tree;
      Index : Package_Index)
      return Cursor
   is
   begin
      return Cursor'(DB    => DB.DB,
                     Index => Index);
   end Position;

   --------------
   -- Position --
   --------------

   function Position (Pos   : Cursor;
                      Index : Package_Index)
                      return Cursor
   is
   begin
      return Cursor'(DB    => Pos.DB,
                     Index => Index);
   end Position;

   --------------
   -- Contains --
   --------------

   function Contains
     (DB   : Package_Tree;
      Name : Package_Name)
      return Boolean
   is
   begin
      return DB.Find (Name) /= No_Element;
   end Contains;

   function To_Cursor_Array (DB   : DB_Access;
                             List : Index_Lists.List)
                             return Cursor_Array
   is
      Result   : Cursor_Array (1 .. Integer (List.Length));
      Position : Index_Lists.Cursor := List.First;
   begin
      for I in Result'Range loop
         Result (I) := (DB, Index_Lists.Element (Position));
         Index_Lists.Next (Position);
      end loop;

      return Result;
   end To_Cursor_Array;

   --------------
   -- Children --
   --------------

   function Children
     (Pos : Cursor)
      return Cursor_Array
   is
   begin
      return To_Cursor_Array (Pos.DB, Pos.DB.Package_Data.Element (Pos.Index).Children);
   end Children;

   ---------------
   -- Is_A_Leaf --
   ---------------

   function Is_A_Leaf
     (Pos : Cursor)
      return Boolean
   is
   begin
      return Pos.DB.Package_Data.Element (Pos.Index).Children.Is_Empty;
   end Is_A_Leaf;

   -----------
   -- Roots --
   -----------

   function Roots
     (DB : Package_Tree)
      return Cursor_Array
   is
   begin
      return To_Cursor_Array (DB.DB, DB.DB.Root_Nodes);
   end Roots;

   -----------------
   -- First_Index --
   -----------------

   function First_Index (DB : Package_Tree) return Package_Index is
   begin
      return DB.DB.Package_Data.First_Index;
   end First_Index;

   ----------
   -- Size --
   ----------

   function Size (DB : Package_Tree) return Natural is
   begin
      return Natural (DB.DB.Package_Data.Length);
   end Size;
   ----------------
   -- Last_Index --
   ----------------

   function Last_Index (DB : Package_Tree) return Package_Index is
   begin
      return DB.DB.Package_Data.Last_Index;
   end Last_Index;

   --------------------
   -- Name_List_Hash --
   --------------------

   function Name_List_Hash
     (Item : Component_List)
      return Ada.Containers.Hash_Type
   is
      use Ada.Containers;
      use Ada.Strings;

      Result : Hash_Type := 0;

      procedure Accumulate (Pos : Package_Lists.Cursor) is
         use Package_Lists;
      begin
         Result := Result + Ada.Strings.Hash (String (Element (Pos)));
      end Accumulate;
   begin
      Item.Iterate (Accumulate'Access);
      return Result;
   end Name_List_Hash;

   ---------
   -- "=" --
   ---------

   function "="
     (Left, Right : Component_List)
      return Boolean
   is
      use type Package_Lists.Cursor;
      use type Ada.Containers.Count_Type;

      Pos_L : Package_Lists.Cursor := Left.First;
      Pos_R : Package_Lists.Cursor := Right.First;
   begin
      if Left.Length /= Right.Length then
         return False;
      else
         while Pos_L /= Package_Lists.No_Element loop
            if Package_Lists.Element (Pos_L) /= Package_Lists.Element (Pos_R) then
               return False;
            end if;

            Package_Lists.Next (Pos_L);
            Package_Lists.Next (Pos_R);
         end loop;
      end if;

      return True;
   end "=";

   procedure Set_Attribute (Pos   : Cursor;
                            Name  : String;
                            Value : String := "")
   is
      procedure Update (Element : in out Package_Descriptor) is
      begin
         Element.Attributes.Include (Key      => Name,
                                     New_Item => Value);
      end Update;
   begin
      Pos.DB.Package_Data.Update_Element (Pos.Index, Update'Access);
   end Set_Attribute;

   function Has_Attribute (Pos   : Cursor;
                           Name  : String)
                           return Boolean
   is
   begin
      return Pos.DB.Package_Data.Element (Pos.Index).Attributes.Contains (Name);
   end Has_Attribute;

   function Get_Attribute (Pos   : Cursor;
                           Name  : String)
                           return String
   is
      use type Attribute_Lists.Cursor;

      P : Attribute_Lists.Cursor;
   begin
      P := Pos.DB.Package_Data.Element (Pos.Index).Attributes.Find (Name);

      if P = Attribute_Lists.No_Element then
         raise Constraint_Error;
      else
         return Attribute_Lists.Element (P);
      end if;
   end Get_Attribute;

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute (Pos     : Cursor;
                           Name    : String;
                           Default : String)
                           return String
   is
      use type Attribute_Lists.Cursor;

      P : Attribute_Lists.Cursor;
   begin
      P := Pos.DB.Package_Data.Element (Pos.Index).Attributes.Find (Name);

      if P = Attribute_Lists.No_Element then
         return Default;
      else
         return Attribute_Lists.Element (P);
      end if;
   end Get_Attribute;

   procedure Attribute_Iterate (Position : Cursor;
                                Process  : access procedure (Name, Value : String))
   is
      procedure Do_It (Pos : Attribute_Lists.Cursor) is
         use Attribute_Lists;
      begin
         Process (Key (Pos), Element (Pos));
      end Do_It;
   begin
      Position.DB.Package_Data.Element (Position.Index).Attributes.Iterate (Do_It'Access);
   end Attribute_Iterate;

   overriding
   procedure Initialize (Obj : in out Package_Tree)
   is
   begin
      Obj.DB := new Basic_DB_Type;
   end Initialize;
end Package_Trees;
