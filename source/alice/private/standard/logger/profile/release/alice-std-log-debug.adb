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
  (Self     : Object;
   Msg      : String;
   Entity   : String := Enclosing_Entity;
   Location : String := Source_Location) is
begin
   null;
end Debug;
