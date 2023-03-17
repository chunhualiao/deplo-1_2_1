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

with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Package_Names is
   type Package_Name is new String;
   --  Type representing the name of a package.  If this seems like
   --  an exageration to you, let me tell you that saved me from a couple
   --  of bugs due to a mix-up between the name of a package and the name
   --  of the correspoding files.

   function Is_Ancestor (Ancestor   : Package_Name;
                         Descendant : Package_Name)
                         return Boolean;
   --  Return true is Descendant is a descendant of Ancestor

   function Is_Valid_Package_Name (Name : String) return Boolean;
   --  Return true if Name is a valid package name.

   -- ============= --
   -- PACKAGE LISTS --
   -- ============= --

   package Package_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Package_Name);

   subtype Package_List is Package_Lists.List;

   function Find_Ancestor (List       : Package_List;
                           Descendant : Package_Name)
                           return Package_Name;
   --  Search List for an ancestor of Descendant and return it, if
   --  found.  If no ancestor is found, return the empty string.
   --  If more than ancestor is present, return the first found.

   function Parse_Package_List (Param : String) return Package_List;
   --  If Param begins with '@', the rest of the string is interpreted
   --  as the name of a file to be read via Read_Package_List; if Param
   --  does not begin with '@', it is expected to be a list of package
   --  names separated by ','.

   function Read_Package_List (Filename : String) return Package_List;
   --  Read a list of packages from the specified filename.  The file
   --  is expected to have one package name per line.  Empty lines and
   --  lines whose first non-space character is '#' are ignored.

   function Force_Prefix_Property (Input : Package_List) return Package_List;
   --  Force the input list to have the "prefix property," that is, no
   --  entry in the list is the prefix of another entry.  (In package
   --  jargon this implies that no package is the ancestor of another
   --  package).  If a package is the ancestor of another, the ancestor
   --  is kept and the descendant is removed.

   -- ================== --
   -- PACKAGE COMPONENTS --
   -- ================== --

   type Component_Name is new String;

   package Component_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Package_Name);

   subtype Component_List is Package_Lists.List;

   function To_Component_List (Item : Package_Name) return Component_List;

   function To_Name (L : Component_List) return Package_Name;

end Package_Names;
