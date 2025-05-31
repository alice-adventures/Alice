-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

procedure Alice is
begin
   Put_Line ("Welcome to the Alice CLI application!");

exception
   when E : others =>
      Put_Line ("An error occurred: " & Exception_Information (E));
end Alice;
