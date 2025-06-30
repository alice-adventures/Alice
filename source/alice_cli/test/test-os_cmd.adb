-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GNAT.Source_Info;

package body Test.OS_Cmd is

   -----------------------------------
   -- Run_OS_Cmd_And_Return_Success --
   -----------------------------------

   procedure Run_OS_Cmd_And_Return_Success
     (Ctx : Alice.Context.Object;
      OS_Cmd : Alice.IFace.OS_Cmd.Object_Access) is
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);
      declare
         Result : constant Alice.IFace.OS_Cmd.Exit_Result'Class :=
           OS_Cmd.Run ("--version", Ctx);
      begin
         null;
      end;
   end Run_OS_Cmd_And_Return_Success;

   ---------------------------------
   -- Run_OS_Cmd_And_Return_Error --
   ---------------------------------

   procedure Run_OS_Cmd_And_Return_Error
     (Ctx : Alice.Context.Object;
      OS_Cmd : Alice.IFace.OS_Cmd.Object_Access) is
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);
      declare
         Result : constant Alice.IFace.OS_Cmd.Exit_Result'Class :=
           OS_Cmd.Run ("--invalid-option", Ctx);
      begin
         null;
      end;
   end Run_OS_Cmd_And_Return_Error;

   -----------------------------------------
   -- Timed_Run_Os_Cmd_And_Finish_In_Time --
   -----------------------------------------

   procedure Timed_Run_Os_Cmd_And_Finish_In_Time
     (Ctx : Alice.Context.Object;
      OS_Cmd : Alice.IFace.OS_Cmd.Object_Access) is
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);
      declare
         Result : Alice.IFace.OS_Cmd.Output_Result'Class :=
           OS_Cmd.Timed_Run ("--version", Ctx, 5.0);
      begin
         null;
      end;
   end Timed_Run_Os_Cmd_And_Finish_In_Time;

   ----------------------------------
   -- Timed_Run_Os_Cmd_And_Timeout --
   ----------------------------------

   procedure Timed_Run_Os_Cmd_And_Timeout
     (Ctx : Alice.Context.Object;
      OS_Cmd : Alice.IFace.OS_Cmd.Object_Access) is
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);
      declare
         -- This command is expected to sleep for 10 seconds, which is longer
         -- than the timeout of 5 seconds.
         -- The command should return an error due to timeout.
         Result : Alice.IFace.OS_Cmd.Output_Result'Class :=
           OS_Cmd.Timed_Run ("--sleep=10", Ctx, 5.0);
      begin
         null;
      end;
   end Timed_Run_Os_Cmd_And_Timeout;

   ---------
   -- Run --
   ---------

   procedure Run (Ctx : Alice.Context.Object) is
   begin
      Run_OS_Cmd_And_Return_Success (Ctx);
      Run_OS_Cmd_And_Return_Error (Ctx);
   end Run;

end Test.OS_Cmd;
