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

--                              -*- Mode: Ada -*-
--  Filename        : tokenize.ads
--  Description     : Ruby-like split
--  Author          : Finta Tartaruga
--  Created On      : Tue Sep 11 22:05:53 2007
--  Last Modified By: R. Bernardini
--  Last Modified On: November 14, 2007
--  Update Count    : 1
--  Status          : <TESTED>

--  This package provides a function Split which divides its input
--  string in smaller strings, separated by a "separator" (much as the
--  split function in Perl, Ruby, and so on...).  Function Split returns
--  a Token_List (defined by this package)  whose elements can be accessed
--  by the function Element.
--
--  Function Split can accept a third Boolean value Collate_Separator.
--  If Collate_Separator is true, consecutive istances of the separator are
--  considered as a single one.  If Collate_Separator is False, for every
--  pair of consecutive separator characters an empty string will be returned.
--  Moreover, if Collate_Separator is True, any separator at the beginning of
--  the string is ignored.  Separators at the end are always ignored.
--
--  The default value of Collate_Separator is true if the separator
--  is the space, false otherwise.
--
--  Examples:
--
--    Split("Hello   there")               returns "Hello" and "there"
--    Split("Hello   there", ' ', False)   returns "Hello", "" and "there"
--    Split("Hello::there", ':')           returns "Hello", "" and "there"
--    Split("Hello::there", ':', True)     returns "Hello" and "there"
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

package Tokenize is
   package String_Vectors is
      new Ada.Containers.Vectors (Index_Type   => Positive,
                                  Element_Type => Unbounded_String);

   subtype Token_List is String_Vectors.Vector;
   type    Token_Array is array (Positive range <>) of Unbounded_String;
   use type Token_List;

   function Split (To_Be_Splitted    : String;
                   Separator         : Character;
                   Collate_Separator : Boolean) return Token_List;
   --  Split string To_Be_Splitted in substring separated by
   --  Separator.  If Collate_Separator is true consider consecutive
   --  istances of Separator as a single one

   function Split (To_Be_Splitted : String;
                   Separator      : Character := ' ') return Token_List;
   --  Similar to the three-parameter version, but the Separator
   --  char defaults to the space and Collate_Separator is True
   --  if Separator is the space, false otherwise

   function Split (To_Be_Splitted : String;
                   Separator      : Character := ' ') return Token_Array;
   --  Like the other two-parameter version, but it returns an array
   --  instead of a vector.  The reason for the presence of this procedure
   --  is mainly historical.

   function Element (Container : Token_List;
                     Index : Positive) return Unbounded_String
                    renames String_Vectors.Element;
   --  Return the Index-th token

   function Length (Container : Token_List) return Natural;
   --  Return the number of tokens

   function To_Array (List : Token_List) return Token_Array;
end Tokenize;
