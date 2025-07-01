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

   ---------------------------------
   -- Run_OS_Cmd_With_Exit_Status --
   ---------------------------------

   procedure Run_OS_Cmd_With_Exit_Result
     (OS_Cmd : Alice.IFace.OS_Cmd.Object_Access;
      Args   : String;
      OS_Ctx : Alice.OS_Context.Object;
      Expect : Alice.Result.Status_Type) is
   begin
      Test.Subtitle ("Run '" & OS_Cmd.Name & " " & Args & "'");

      Result : constant Alice.IFace.OS_Cmd.Exit_Result'Class :=
        OS_Cmd.Run (Args, OS_Ctx);

      if Result.Status = Expect then
         Test.Pass;
      else
         Test.Fail ("Exit status:" & Result.Exit_Status'Image);
      end if;

   exception
      when E : others =>
         OS_Ctx.Log.Warning
           ("Exception caught: " & Ada.Exceptions.Exception_Information (E));
         Test.Fail;
   end Run_OS_Cmd_With_Exit_Result;

   -------------------------
   -- Check_Result_Output --
   -------------------------

   procedure Check_Result_Output
     (OS_Cmd : Alice.IFace.OS_Cmd.Object_Access;
      Result : in out Alice.IFace.OS_Cmd.Output_Result'Class;
      OS_Ctx : Alice.OS_Context.Object;
      Expect : Alice.Result.Status_Type) is
   begin
      if Result.Status = Expect then
         OS_Ctx.Log.Save_State;
         OS_Ctx.Log.Set_Debug_Level (With_Location_Enabled => False);
         OS_Cmd.Debug_Output_Result (Result, OS_Ctx);
         OS_Ctx.Log.Restore_State;

         Clean_Result : Alice.Result.Object'Class :=
           OS_Cmd.Cleanup (Result, OS_Ctx);

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
         OS_Ctx.Log.Warning
           ("Exception caught: " & Ada.Exceptions.Exception_Information (E));
         Test.Fail;
   end Check_Result_Output;

   -----------------------------------
   -- Run_OS_Cmd_With_Output_Result --
   -----------------------------------

   procedure Run_OS_Cmd_With_Output_Result
     (OS_Cmd : Alice.IFace.OS_Cmd.Object_Access;
      Args   : String;
      OS_Ctx : Alice.OS_Context.Object;
      Expect : Alice.Result.Status_Type) is
   begin
      Test.Subtitle ("Run '" & OS_Cmd.Name & " " & Args & "'");

      Result : Alice.IFace.OS_Cmd.Output_Result'Class :=
        OS_Cmd.Run (Args, OS_Ctx);
      Check_Result_Output (OS_Cmd, Result, OS_Ctx, Expect);

   exception
      when E : others =>
         OS_Ctx.Log.Warning
           ("Exception caught: " & Ada.Exceptions.Exception_Information (E));
         Test.Fail;
   end Run_OS_Cmd_With_Output_Result;

   ------------------------------------
   -- Run_OS_Cmd_With_Timeout_Result --
   ------------------------------------

   procedure Run_OS_Cmd_With_Timeout_Result
     (OS_Cmd  : Alice.IFace.OS_Cmd.Object_Access;
      Args    : String;
      OS_Ctx  : Alice.OS_Context.Object;
      Timeout : Duration;
      Expect  : Alice.Result.Status_Type) is
   begin
      Test.Subtitle ("Timed Run '" & OS_Cmd.Name & " " & Args & "'");

      Result : Alice.IFace.OS_Cmd.Output_Result'Class :=
        OS_Cmd.Timed_Run (Args, OS_Ctx, Timeout);
      Check_Result_Output (OS_Cmd, Result, OS_Ctx, Expect);

   exception
      when E : others =>
         OS_Ctx.Log.Warning
           ("Exception caught: " & Ada.Exceptions.Exception_Information (E));
         Test.Fail;
   end Run_OS_Cmd_With_Timeout_Result;

   -----------------------------------
   -- Run_OS_Cmd_And_Return_Success --
   -----------------------------------

   procedure Run_OS_Cmd_And_Return_Success
     (OS_Cmd : Alice.Context.OS_Commands; OS_Ctx : Alice.OS_Context.Object)
   is
      Args   : constant String := "--version";
      Expect : constant Alice.Result.Status_Type := Alice.Result.Success;
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);

      Run_OS_Cmd_With_Exit_Result (OS_Cmd.Alr, Args, OS_Ctx, Expect);
      Run_OS_Cmd_With_Exit_Result (OS_Cmd.Curl, Args, OS_Ctx, Expect);
      Run_OS_Cmd_With_Exit_Result (OS_Cmd.Git, Args, OS_Ctx, Expect);
   end Run_OS_Cmd_And_Return_Success;

   ---------------------------------
   -- Run_OS_Cmd_And_Return_Error --
   ---------------------------------

   procedure Run_OS_Cmd_And_Return_Error
     (OS_Cmd : Alice.Context.OS_Commands; OS_Ctx : Alice.OS_Context.Object)
   is
      Args   : constant String := "--invalid-option";
      Expect : constant Alice.Result.Status_Type := Alice.Result.Error;
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);

      Run_OS_Cmd_With_Exit_Result (OS_Cmd.Alr, Args, OS_Ctx, Expect);
      Run_OS_Cmd_With_Exit_Result (OS_Cmd.Curl, Args, OS_Ctx, Expect);
      Run_OS_Cmd_With_Exit_Result (OS_Cmd.Git, Args, OS_Ctx, Expect);
   end Run_OS_Cmd_And_Return_Error;

   -----------------------------------
   -- Run_OS_Cmd_And_Output_Success --
   -----------------------------------

   procedure Run_OS_Cmd_And_Output_Success
     (OS_Cmd : Alice.Context.OS_Commands; OS_Ctx : Alice.OS_Context.Object)
   is
      Args   : constant String := "--version";
      Expect : constant Alice.Result.Status_Type := Alice.Result.Success;
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);

      Run_OS_Cmd_With_Output_Result (OS_Cmd.Alr, Args, OS_Ctx, Expect);
      Run_OS_Cmd_With_Output_Result (OS_Cmd.Curl, Args, OS_Ctx, Expect);
      Run_OS_Cmd_With_Output_Result (OS_Cmd.Git, Args, OS_Ctx, Expect);
   end Run_OS_Cmd_And_Output_Success;

   ---------------------------------
   -- Run_OS_Cmd_And_Output_Error --
   ---------------------------------

   procedure Run_OS_Cmd_And_Output_Error
     (OS_Cmd : Alice.Context.OS_Commands; OS_Ctx : Alice.OS_Context.Object)
   is
      Args   : constant String := "--invalid-option";
      Expect : constant Alice.Result.Status_Type := Alice.Result.Error;
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);

      Run_OS_Cmd_With_Output_Result (OS_Cmd.Alr, Args, OS_Ctx, Expect);
      Run_OS_Cmd_With_Output_Result (OS_Cmd.Curl, Args, OS_Ctx, Expect);
      Run_OS_Cmd_With_Output_Result (OS_Cmd.Git, Args, OS_Ctx, Expect);
   end Run_OS_Cmd_And_Output_Error;

   -----------------------------------------
   -- Timed_Run_Os_Cmd_And_Finish_In_Time --
   -----------------------------------------

   procedure Timed_Run_Os_Cmd_And_Finish_In_Time
     (OS_Cmd : Alice.Context.OS_Commands; OS_Ctx : Alice.OS_Context.Object)
   is
      Args    : constant String := "--version";
      Timeout : constant Duration := 1.0;
      Expect  : constant Alice.Result.Status_Type := Alice.Result.Success;
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);

      Run_OS_Cmd_With_Timeout_Result
        (OS_Cmd.Alr, Args, OS_Ctx, Timeout, Expect);
      Run_OS_Cmd_With_Timeout_Result
        (OS_Cmd.Curl, Args, OS_Ctx, Timeout, Expect);
      Run_OS_Cmd_With_Timeout_Result
        (OS_Cmd.Git, Args, OS_Ctx, Timeout, Expect);
   end Timed_Run_Os_Cmd_And_Finish_In_Time;

   ----------------------------------
   -- Timed_Run_Os_Cmd_And_Timeout --
   ----------------------------------

   procedure Timed_Run_Os_Cmd_And_Timeout
     (OS_Cmd : Alice.Context.OS_Commands; OS_Ctx : Alice.OS_Context.Object)
   is
      Expect : constant Alice.Result.Status_Type := Alice.Result.Error;
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);

      Run_OS_Cmd_With_Timeout_Result
        (OS_Cmd.Alr, "--no-tty init", OS_Ctx, 0.001, Expect);

      Run_OS_Cmd_With_Timeout_Result
        (OS_Cmd.Curl,
         "https://distrib-coffee.ipsl.jussieu.fr"
         & "/pub/linux/ubuntu-releases/25.04/ubuntu-25.04-desktop-amd64.iso "
         & "--output /dev/null",
         OS_Ctx,
         1.0,
         Expect);

      Run_OS_Cmd_With_Timeout_Result
        (OS_Cmd.Git, "remote --verbose update origin", OS_Ctx, 0.01, Expect);
   end Timed_Run_Os_Cmd_And_Timeout;

   ---------
   -- Run --
   ---------

   procedure Run (Ctx : Alice.Context.Object; OS_Ctx : Alice.OS_Context.Object)
   is
   begin
      Run_OS_Cmd_And_Return_Success (Ctx.OS_Cmd, OS_Ctx);
      Run_OS_Cmd_And_Return_Error (Ctx.OS_Cmd, OS_Ctx);
      Run_OS_Cmd_And_Output_Success (Ctx.OS_Cmd, OS_Ctx);
      Run_OS_Cmd_And_Output_Error (Ctx.OS_Cmd, OS_Ctx);
      Timed_Run_Os_Cmd_And_Finish_In_Time (Ctx.OS_Cmd, OS_Ctx);
      Timed_Run_Os_Cmd_And_Timeout (Ctx.OS_Cmd, OS_Ctx);

   end Run;

end Test.OS_Cmd;
