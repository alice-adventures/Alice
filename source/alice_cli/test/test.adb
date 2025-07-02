-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO;

with AnsiAda;

with Alice.Std;

package body Test is

   package ANSI renames AnsiAda;

   Last_Title : Boolean := False;

   -----------
   -- Title --
   -----------

   procedure Title (Title : String) is
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line
        (ANSI.Wrap
           (" -- " & Title & " -- ",
            ANSI.Invert,
            ANSI.Foreground (ANSI.Light_Cyan)));
      Last_Title := True;
   end Title;

   --------------
   -- Subtitle --
   --------------

   procedure Subtitle (Subtitle : String) is
   begin
      if not Last_Title then
         Ada.Text_IO.New_Line;
      end if;
      Last_Title := False;

      Ada.Text_IO.Put_Line
        (ANSI.Wrap
           ("-- " & Subtitle,
            ANSI.Bright,
            ANSI.Foreground (ANSI.Cyan)));
   end Subtitle;

   -------------
   -- Success --
   -------------

   procedure Pass is
   begin
      Ada.Text_IO.Put_Line
        (ANSI.Wrap ("[ PASS ]", ANSI.Invert, ANSI.Foreground (ANSI.Green)));
   end Pass;

   -------------
   -- Failure --
   -------------

   procedure Fail (Message : String := "") is
   begin
      Ada.Text_IO.Put_Line
        (ANSI.Wrap ("[ FAIL ]", ANSI.Invert, ANSI.Foreground (ANSI.Red)));
      Alice.Std.Get_OS_Context.Log.Warning (Message);
   end Fail;

end Test;
