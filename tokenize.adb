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
package body Tokenize is

   --  Called by Split when the separator is a space.  According to
   --  Ruby convention, space is a special separator in the sense
   --  that consecutive spaces do not give rise to empty tokens.
   function Collated_Split (To_Be_Splitted : String;
                            Separator      : Character)
                            return Token_List is
      Result         : Token_List := String_Vectors.Empty_Vector;
      Current, First : Integer;
   begin

      Current := To_Be_Splitted'First;

      Main_Loop :
      while Current <= To_Be_Splitted'Last loop

         Search_For_Begin :
         --  Since we are doing a Collated split, we need to skip
         --  all the separators
         while Current <= To_Be_Splitted'Last and then
           To_Be_Splitted (Current) = Separator loop
            Current := Current + 1;
         end loop Search_For_Begin;

         --  If I am here or Current points after the end of
         --  the string of To_Be_Splitted(Current) is a non-sep
         --  character

         exit Main_Loop when Current > To_Be_Splitted'Last;

         --  If I am here, To_Be_Splitted(Current) is a
         --  non-separator character

         First := Current;

         Search_For_End :
         while Current <= To_Be_Splitted'Last and then
           To_Be_Splitted (Current) /= Separator loop
            Current := Current + 1;
         end loop Search_For_End;

         String_Vectors.Append (Result,
                                To_Unbounded_String (To_Be_Splitted (First .. Current - 1)));

         Current := Current + 1;
      end loop Main_Loop;

      return Result;
   end Collated_Split;

   -----------
   -- Split --
   -----------

   function Uncollated_Split (To_Be_Splitted : String;
                              Separator      : Character)
                              return Token_List is
      Result : Token_List := String_Vectors.Empty_Vector;

      Current, First : Integer;
   begin
      Current := To_Be_Splitted'First;

      Main_Loop :
      while Current <= To_Be_Splitted'Last loop
         First := Current;

         Search_For_End :
         while Current <= To_Be_Splitted'Last and then
           To_Be_Splitted (Current) /= Separator loop
            Current := Current + 1;
         end loop Search_For_End;

         String_Vectors.Append (Result,
                                To_Unbounded_String (To_Be_Splitted (First .. Current - 1)));

         if Current = To_Be_Splitted'Last then
            String_Vectors.Append (Result, Null_Unbounded_String);
         end if;

         Current := Current + 1;
      end loop Main_Loop;

      return Result;
   end Uncollated_Split;

   function Split (To_Be_Splitted    : String;
                   Separator         : Character;
                   Collate_Separator : Boolean)
                   return Token_List is
   begin
      if Collate_Separator then
         return Collated_Split (To_Be_Splitted, Separator);
      else
         return Uncollated_Split (To_Be_Splitted, Separator);
      end if;
   end Split;

   --  Similar to the three-parameter version, but the Separator
   --  char defaults to the space and Collate_Separator is True
   --  if Separator is the space, false otherwise
   function Split (To_Be_Splitted : String;
                   Separator      : Character := ' ')
                   return Token_List is
   begin
      if Separator = ' ' then
         return Collated_Split (To_Be_Splitted, Separator);
      else
         return Uncollated_Split (To_Be_Splitted, Separator);
      end if;
   end Split;

   function Split (To_Be_Splitted : String;
                   Separator      : Character := ' ') return Token_Array
   is
   begin
      return To_Array (Split (To_Be_Splitted, Separator));
   end Split;

   function Length (Container : Token_List) return Natural is
   begin
      return Natural (String_Vectors.Length (Container));
   end Length;

   function To_Array (List : Token_List)
                      return Token_Array is
      Result : Token_Array (1 .. Length (List));
   begin
      for I in Result'Range loop
         Result (I) := Element (List, I);
      end loop;

      return Result;
   end To_Array;
end Tokenize;
