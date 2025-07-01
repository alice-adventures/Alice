-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.Result;
with GNAT.Source_Info;

with Alice.IFace.OS_Cmd;

package body Test.OS_Cmd is

   ---------------------------------
   -- Run_OS_Cmd_With_Exit_Status --
   ---------------------------------

   procedure Run_OS_Cmd_With_Exit_Result
     (OS_Cmd : Alice.IFace.OS_Cmd.Object_Access;
      Args   : String;
      OS_Ctx : Alice.OS_Context.Object;
      Expect : Alice.Result.Status_Type)
   is
      use all type Alice.Result.Status_Type;
   begin
      Test.Subtitle ("Run '" & OS_Cmd.Name & " " & Args & "'");

      Result : constant Alice.IFace.OS_Cmd.Exit_Result'Class :=
        OS_Cmd.Run (Args, OS_Ctx);

      if Result.Status = Expect then
         Test.Pass;
      else
         Test.Fail ("Exit status:" & Result.Exit_Status'Image);
      end if;
   end Run_OS_Cmd_With_Exit_Result;

   procedure Run_OS_Cmd_With_Output_Result
     (OS_Cmd : Alice.IFace.OS_Cmd.Object_Access;
      Args   : String;
      OS_Ctx : Alice.OS_Context.Object;
      Expect : Alice.Result.Status_Type)
   is
      use all type Alice.Result.Status_Type;
   begin
      Test.Subtitle ("Run '" & OS_Cmd.Name & " " & Args & "'");

      Result : constant Alice.IFace.OS_Cmd.Output_Result'Class :=
        OS_Cmd.Run (Args, OS_Ctx);

      if Result.Status = Expect then
         Test.Pass;
      else
         Fail ("Exit status:" & Result.Exit_Status'Image);
      end if;
   end Run_OS_Cmd_With_Output_Result;

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

      Run_OS_Cmd_With_Exit_Result (Os_Cmd.Alr, Args, OS_Ctx, Expect);
      Run_OS_Cmd_With_Exit_Result (Os_Cmd.Curl, Args, OS_Ctx, Expect);
      Run_OS_Cmd_With_Exit_Result (Os_Cmd.Git, Args, OS_Ctx, Expect);
   end Run_OS_Cmd_And_Return_Error;

   -----------------------------------------
   -- Timed_Run_Os_Cmd_And_Finish_In_Time --
   -----------------------------------------

   procedure Timed_Run_Os_Cmd_And_Finish_In_Time
     (OS_Cmd : Alice.IFace.OS_Cmd.Object_Access;
      Args   : String;
      OS_Ctx : Alice.OS_Context.Object) is
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);

      Result : Alice.IFace.OS_Cmd.Output_Result'Class :=
        OS_Cmd.Timed_Run ("--version", OS_Ctx, 5.0);

   end Timed_Run_Os_Cmd_And_Finish_In_Time;

   ----------------------------------
   -- Timed_Run_Os_Cmd_And_Timeout --
   ----------------------------------

   procedure Timed_Run_Os_Cmd_And_Timeout
     (OS_Cmd : Alice.IFace.OS_Cmd.Object_Access;
      Args   : String;
      OS_Ctx : Alice.OS_Context.Object) is
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);

      -- This command is expected to sleep for 10 seconds, which is longer
      -- than the timeout of 5 seconds. The command should return an error due
      -- to timeout.
      Result : Alice.IFace.OS_Cmd.Output_Result'Class :=
        OS_Cmd.Timed_Run ("--sleep=10", OS_Ctx, 5.0);

   end Timed_Run_Os_Cmd_And_Timeout;

   ---------
   -- Run --
   ---------

   procedure Run (Ctx : Alice.Context.Object; OS_Ctx : Alice.OS_Context.Object)
   is
   begin
      Run_OS_Cmd_And_Return_Success (Ctx.OS_Cmd, OS_Ctx);
      Run_OS_Cmd_And_Return_Error (Ctx.OS_Cmd, OS_Ctx);
   --  Timed_Run_Os_Cmd_And_Finish_In_Time (Ctx.OS_Cmd.Curl, "--version", OS_Ctx);
   --  Timed_Run_Os_Cmd_And_Timeout (Ctx.OS_Cmd.Curl, "--sleep=10", OS_Ctx);

   end Run;

end Test.OS_Cmd;
