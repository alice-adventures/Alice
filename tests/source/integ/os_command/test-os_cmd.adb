-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Exceptions;

with Alice.IFace.OS_Cmd;
with Alice.Result;
with GNAT.Source_Info;

package body Test.OS_Cmd is

   use all type Alice.Result.Status_Type;

   --  #REVIEW - Refactor this code to use a more structured approach for
   --  running OS commands and checking results. The current implementation is
   --  repetitive and could benefit from a more modular design based on the
   --  example provided in the Test.VCS.Profile and Test.VCS.Service packages.

   package Run is

      procedure With_Given_Args_Succeeds (OS_Cmd : Alice.Context.OS_Commands);

      procedure With_Given_Args_Fails (OS_Cmd : Alice.Context.OS_Commands);

      procedure With_Given_Args_Output_Succeeds
        (OS_Cmd : Alice.Context.OS_Commands);

      procedure With_Given_Args_Output_Fails
        (OS_Cmd : Alice.Context.OS_Commands);

      procedure With_Given_Args_Timed_Output_Succeeds_In_Time
        (OS_Cmd : Alice.Context.OS_Commands);

      procedure With_Given_Args_Timed_Output_Exceeds_Timeout
        (OS_Cmd : Alice.Context.OS_Commands);

   end Run;

   --------------------------
   -- Run_With_Exit_Result --
   --------------------------

   procedure Run_With_Exit_Result
     (OS_Cmd : Alice.IFace.OS_Cmd.Object_Access;
      Args   : String;
      Expect : Alice.Result.Status_Type)
   is
      Result : constant Alice.IFace.OS_Cmd.Result_Exit'Class :=
        OS_Cmd.Run (Args);
   begin
      Test.Subtitle
        ("Run '" & OS_Cmd.Name & " " & Args & "' expects " & Expect'Image);

      if Result.Status = Expect then
         Test.Pass;
      else
         Test.Fail ("Exit status:" & Result.Exit_Status'Image);
      end if;

   exception
      when E : others =>
         OS_Cmd.Context.Log.Warning
           ("Exception caught: " & Ada.Exceptions.Exception_Information (E));
         Test.Fail;
   end Run_With_Exit_Result;

   -------------------------
   -- Check_Result_Output --
   -------------------------

   procedure Check_Result_Output
     (OS_Cmd : Alice.IFace.OS_Cmd.Object_Access;
      Result : in out Alice.IFace.OS_Cmd.Result_Output'Class;
      Expect : Alice.Result.Status_Type) is
   begin
      if Result.Status = Expect then
         OS_Cmd.Context.Log.Save_State;
         OS_Cmd.Context.Log.Set_Debug_Level (With_Location_Enabled => False);
         OS_Cmd.Debug_Output_Result (Result);
         OS_Cmd.Context.Log.Restore_State;

         Clean_Result : constant Alice.Result.Object'Class :=
           OS_Cmd.Cleanup (Result);

         if Clean_Result.Status = Alice.Result.Success then
            Test.Pass;
         else
            Test.Fail ("Cleanup failed: " & Alice.Str (Clean_Result.Message));
         end if;
      else
         Fail ("Exit status:" & Result.Exit_Status'Image);
      end if;

   exception
      when E : others =>
         OS_Cmd.Context.Log.Warning
           ("Exception caught: " & Ada.Exceptions.Exception_Information (E));
         Test.Fail;
   end Check_Result_Output;

   ----------------------------
   -- Run_With_Output_Result --
   ----------------------------

   procedure Run_With_Output_Result
     (OS_Cmd : Alice.IFace.OS_Cmd.Object_Access;
      Args   : String;
      Expect : Alice.Result.Status_Type) is
   begin
      Test.Subtitle
        ("Run '" & OS_Cmd.Name & " " & Args & "' expects " & Expect'Image);

      declare
         Result : Alice.IFace.OS_Cmd.Result_Output'Class := OS_Cmd.Run (Args);
      begin
         Check_Result_Output (OS_Cmd, Result, Expect);
      end;

   exception
      when E : others =>
         OS_Cmd.Context.Log.Warning
           ("Exception caught: " & Ada.Exceptions.Exception_Information (E));
         Test.Fail;
   end Run_With_Output_Result;

   ---------------------------
   -- Run_With_Timed_Output --
   ---------------------------

   procedure Run_With_Timed_Output
     (OS_Cmd  : Alice.IFace.OS_Cmd.Object_Access;
      Args    : String;
      Timeout : Duration;
      Expect  : Alice.Result.Status_Type) is
   begin
      Test.Subtitle ("Timed Run '" & OS_Cmd.Name & " " & Args & "'");

      declare
         Result : Alice.IFace.OS_Cmd.Result_Output'Class :=
           OS_Cmd.Timed_Run (Args, Timeout);
      begin
         Check_Result_Output (OS_Cmd, Result, Expect);
      end;

   exception
      when E : others =>
         OS_Cmd.Context.Log.Warning
           ("Exception caught: " & Ada.Exceptions.Exception_Information (E));
         Test.Fail;
   end Run_With_Timed_Output;

   package body Run is

      ------------------------------
      -- With_Given_Args_Succeeds --
      ------------------------------

      procedure With_Given_Args_Succeeds (OS_Cmd : Alice.Context.OS_Commands)
      is
         Args   : constant String := "--version";
         Expect : constant Alice.Result.Status_Type := Alice.Result.Success;
      begin
         Test.Title (GNAT.Source_Info.Enclosing_Entity);

         Run_With_Exit_Result (OS_Cmd.Alr, Args, Expect);
         Run_With_Exit_Result (OS_Cmd.Curl, Args, Expect);
         Run_With_Exit_Result (OS_Cmd.Git, Args, Expect);
      end With_Given_Args_Succeeds;

      ---------------------------
      -- With_Given_Args_Fails --
      ---------------------------

      procedure With_Given_Args_Fails (OS_Cmd : Alice.Context.OS_Commands) is
         Args   : constant String := "--invalid-option";
         Expect : constant Alice.Result.Status_Type := Alice.Result.Error;
      begin
         Test.Title (GNAT.Source_Info.Enclosing_Entity);

         Run_With_Exit_Result (OS_Cmd.Alr, Args, Expect);
         Run_With_Exit_Result (OS_Cmd.Curl, Args, Expect);
         Run_With_Exit_Result (OS_Cmd.Git, Args, Expect);
      end With_Given_Args_Fails;

      -------------------------------------
      -- With_Given_Args_Output_Succeeds --
      -------------------------------------

      procedure With_Given_Args_Output_Succeeds
        (OS_Cmd : Alice.Context.OS_Commands)
      is
         Args   : constant String := "--version";
         Expect : constant Alice.Result.Status_Type := Alice.Result.Success;
      begin
         Test.Title (GNAT.Source_Info.Enclosing_Entity);

         Run_With_Output_Result (OS_Cmd.Alr, Args, Expect);
         Run_With_Output_Result (OS_Cmd.Curl, Args, Expect);
         Run_With_Output_Result (OS_Cmd.Git, Args, Expect);
      end With_Given_Args_Output_Succeeds;

      ----------------------------------
      -- With_Given_Args_Output_Fails --
      ----------------------------------

      procedure With_Given_Args_Output_Fails
        (OS_Cmd : Alice.Context.OS_Commands)
      is
         Args   : constant String := "--invalid-option";
         Expect : constant Alice.Result.Status_Type := Alice.Result.Error;
      begin
         Test.Title (GNAT.Source_Info.Enclosing_Entity);

         Run_With_Output_Result (OS_Cmd.Alr, Args, Expect);
         Run_With_Output_Result (OS_Cmd.Curl, Args, Expect);
         Run_With_Output_Result (OS_Cmd.Git, Args, Expect);
      end With_Given_Args_Output_Fails;

      ---------------------------------------------------
      -- With_Given_Args_Timed_Output_Succeeds_In_Time --
      ---------------------------------------------------

      procedure With_Given_Args_Timed_Output_Succeeds_In_Time
        (OS_Cmd : Alice.Context.OS_Commands)
      is
         Args    : constant String := "--version";
         Timeout : constant Duration := 1.0;
         Expect  : constant Alice.Result.Status_Type := Alice.Result.Success;
      begin
         Test.Title (GNAT.Source_Info.Enclosing_Entity);

         Run_With_Timed_Output (OS_Cmd.Alr, Args, Timeout, Expect);
         Run_With_Timed_Output (OS_Cmd.Curl, Args, Timeout, Expect);
         Run_With_Timed_Output (OS_Cmd.Git, Args, Timeout, Expect);
      end With_Given_Args_Timed_Output_Succeeds_In_Time;

      --------------------------------------------------
      -- With_Given_Args_Timed_Output_Exceeds_Timeout --
      --------------------------------------------------

      procedure With_Given_Args_Timed_Output_Exceeds_Timeout
        (OS_Cmd : Alice.Context.OS_Commands)
      is
         Expect : constant Alice.Result.Status_Type := Alice.Result.Error;
      begin
         Test.Title (GNAT.Source_Info.Enclosing_Entity);

         Run_With_Timed_Output (OS_Cmd.Alr, "--no-tty init", 0.001, Expect);

         Run_With_Timed_Output
           (OS_Cmd.Curl,
            "https://distrib-coffee.ipsl.jussieu.fr"
            & "/pub/linux/ubuntu-releases/25.04/ubuntu-25.04-desktop-amd64.iso "
            & "--output /dev/null",
            0.05,
            Expect);

         Run_With_Timed_Output
           (OS_Cmd.Git, "remote --verbose update origin", 0.01, Expect);
      end With_Given_Args_Timed_Output_Exceeds_Timeout;

   end Run;

   -------------------
   -- Run_All_Tests --
   -------------------

   procedure Run_All_Tests (Context : Alice.Context.Object_Access) is
   begin
      Run.With_Given_Args_Succeeds (Context.OS_Cmd);
      Run.With_Given_Args_Fails (Context.OS_Cmd);
      Run.With_Given_Args_Output_Succeeds (Context.OS_Cmd);
      Run.With_Given_Args_Output_Fails (Context.OS_Cmd);
      Run.With_Given_Args_Timed_Output_Succeeds_In_Time (Context.OS_Cmd);
      Run.With_Given_Args_Timed_Output_Exceeds_Timeout (Context.OS_Cmd);
   end Run_All_Tests;

end Test.OS_Cmd;
