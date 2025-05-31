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

procedure Alice_CLI is
begin
   Alice.Log.Info ("Starting Alice CLI application...");
   Put_Line ("Welcome to the Alice " & Alice.Version & " CLI application!");

exception
   when E : others =>
      Put_Line ("An error occurred: " & Exception_Information (E));
end Alice_CLI;
