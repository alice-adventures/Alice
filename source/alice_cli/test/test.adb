-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO;

with Alice.Std;

package body Test is

   Main_Color : ANSI.Colors := ANSI.Light_Cyan;
   Last_Title : Boolean := False;

   -------------
   -- Section --
   -------------

   procedure Section (Section : String; Color : ANSI.Colors) is
      Spaces : constant String :=
        "                                                               ";
   begin
      Main_Color := Color;

      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put_Line
        (ANSI.Wrap
           (" ----------------------------------"
            & "---------------------------------- ",
            ANSI.Invert,
            ANSI.Foreground (Main_Color)));
      Ada.Text_IO.Put_Line
        (ANSI.Wrap
           (" --  "
            & Section
            & Spaces (2 .. Spaces'Length - Section'Length)
            & "-- ",
            ANSI.Invert,
            ANSI.Foreground (Main_Color)));
      Ada.Text_IO.Put_Line
        (ANSI.Wrap
           (" ----------------------------------"
            & "---------------------------------- ",
            ANSI.Invert,
            ANSI.Foreground (Main_Color)));
   end Section;

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
            ANSI.Foreground (Main_Color)));
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
           ("-- " & Subtitle, ANSI.Bright, ANSI.Foreground (Main_Color)));
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

   -----------
   -- Error --
   -----------

   procedure Error (Message : String) is
   begin
      Ada.Text_IO.Put_Line
        (ANSI.Wrap
           ("[ ERROR ]",
            ANSI.Default,
            ANSI.Palette_Fg (5, 5, 5),
            ANSI.Palette_Bg (3, 0, 0)));
      Alice.Std.Get_OS_Context.Log.Warning (Message & " (?)");
   end Error;

end Test;
