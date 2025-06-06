-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This file contains the definition of the Alice package, the top-level
--  package. It provides utilities for working with unbounded strings. It
--  includes functions to convert between `String` and `Unbounded_String`,
--  allowing for flexible string manipulation without worrying about fixed
--  sizes.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Alice is

   subtype UString is Unbounded_String;
   --  A subtype of `Unbounded_String` for convenience.

   function UStr (Source : String) return Unbounded_String
   renames To_Unbounded_String;
   --  Convert a `String` to an `Unbounded_String`.

   function Str (Source : Unbounded_String) return String renames To_String;
   --  Convert an `Unbounded_String` to a `String`.

end Alice;
