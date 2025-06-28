-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO;

with AnsiAda;

package body Test is

   package ANSI renames AnsiAda;

   procedure Title (Title : String) is
   begin
      Ada.Text_IO.Put_Line
        (ANSI.Wrap
           (" -- " & Title & " -- ",
            ANSI.Invert,
            ANSI.Foreground (ANSI.Cyan)));
   end Title;

end Test;
