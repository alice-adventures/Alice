-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Alice is

   subtype UString is Unbounded_String;

   function U (Source : String) return Unbounded_String
   renames To_Unbounded_String;

   function Str (Source : Unbounded_String) return String renames To_String;

end Alice;
