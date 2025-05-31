-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Alice;
with Alice.Log;

procedure Alice_CLI is
begin
   Alice.Log.Optimize_For_CLI;
   Alice.Log.Set_Debug_Level;

   Alice.Log.Trace_Begin ("Alice_CLI");
   Put_Line ("Welcome to the Alice " & Alice.Version & " CLI application!");
   Alice.Log.Trace_End ("Alice_CLI");

exception
   when E : others =>
      Alice.Log.Error ("Exception caught: " & Exception_Information (E));
end Alice_CLI;
