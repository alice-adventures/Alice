-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

separate (Alice.Log)
procedure Trace_Return
  (Msg      : String := "";
   Entity   : String := Enclosing_Entity;
   Location : String := Source_Location) is
begin
   Simple_Logging.Detail ("RETURN " & Entity & " " & Msg, Entity, Location);
end Trace_Return;
