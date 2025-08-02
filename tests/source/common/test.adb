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

   use all type Alice.Result.Status_Type;

   Main_Color : ANSI.Colors := ANSI.Light_Cyan;
   Last_Title : Boolean := False;

   Σ_Tests      : Natural := 0;
   Σ_Pass_Tests : Natural := 0;
   Σ_Fail_Tests : Natural := 0;
   Σ_Warnings   : Natural := 0;
   Σ_Errors     : Natural := 0;

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

   ----------
   -- Pass --
   ----------

   procedure Pass is
   begin
      Σ_Tests := Σ_Tests + 1;
      Σ_Pass_Tests := Σ_Pass_Tests + 1;
      Ada.Text_IO.Put_Line
        (ANSI.Wrap
           (" PASS ",
            ANSI.Default,
            ANSI.Palette_Fg (5, 5, 5),
            ANSI.Palette_Bg (0, 2, 0)));
   end Pass;

   ----------
   -- Fail --
   ----------

   procedure Fail (Message : String := "") is
   begin
      Σ_Tests := Σ_Tests + 1;
      Σ_Fail_Tests := Σ_Fail_Tests + 1;
      Ada.Text_IO.Put_Line
        (ANSI.Wrap
           (" FAIL ",
            ANSI.Default,
            ANSI.Palette_Fg (5, 5, 5),
            ANSI.Palette_Bg (2, 0, 0)));
      Alice.Std.Get_OS_Context.Err.Log (Message);
   end Fail;

   -------------
   -- Warning --
   -------------

   procedure Warning (Message : String) is
   begin
      Σ_Tests := Σ_Tests + 1;
      Σ_Warnings := Σ_Warnings + 1;
      Ada.Text_IO.Put_Line
        (ANSI.Wrap
           (" WARNING ",
            ANSI.Default,
            ANSI.Palette_Fg (5, 5, 5),
            ANSI.Palette_Bg (3, 1, 0)));
      Alice.Std.Get_OS_Context.Log.Warning (Message);
   end Warning;

   -----------
   -- Error --
   -----------

   procedure Error (Status : Alice.Result.Status_Type) is
   begin
      Σ_Tests := Σ_Tests + 1;
      Σ_Errors := Σ_Errors + 1;
      Ada.Text_IO.Put_Line
        (ANSI.Wrap
           (" ERROR ",
            ANSI.Default,
            ANSI.Palette_Fg (5, 5, 5),
            ANSI.Palette_Bg (2, 0, 0)));
      Alice.Std.Get_OS_Context.Err.Log
        ("This test should have "
         & (if Status = Alice.Result.Success then "failed" else "succeeded")
         & " (?)");
   end Error;

   -------------
   -- Summary --
   -------------

   procedure Summary is
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;
      Section ("SUMMARY", ANSI.Light_Green);

      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line
        (ANSI.Wrap
           ("Summary:"
            & Σ_Tests'Image
            & " test"
            & (if Σ_Tests = 1 then "" else "s")
            & " run,"
            & Σ_Pass_Tests'Image
            & " passed,"
            & Σ_Fail_Tests'Image
            & " failed, with"
            & Σ_Warnings'Image
            & " warning"
            & (if Σ_Warnings = 1 then "" else "s")
            & " and"
            & Σ_Errors'Image
            & " error"
            & (if Σ_Errors = 1 then "" else "s"),
            ANSI.Bright,
            ANSI.Foreground (ANSI.Light_Green)));
      Ada.Text_IO.New_Line;
      if Σ_Fail_Tests > 0 or else Σ_Errors > 0 then
         Ada.Text_IO.Put_Line
           (ANSI.Wrap
              ("Some tests failed or had errors, check the log for details",
               ANSI.Bright,
               ANSI.Foreground (ANSI.Red)));
      else
         Ada.Text_IO.Put_Line
           (ANSI.Wrap
              ("All tests passed successfully!",
               ANSI.Bright,
               ANSI.Foreground (ANSI.Green)));
      end if;
      Ada.Text_IO.New_Line;
   end Summary;

end Test;
