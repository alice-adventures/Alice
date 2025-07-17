-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

separate (Alice.Std.Log)
overriding
procedure Debug
  (Self     : in out Object;
   Message  : String;
   Entity   : String := GNAT.Source_Info.Enclosing_Entity;
   Location : String := GNAT.Source_Info.Source_Location) is
begin
   null;
end Debug;
