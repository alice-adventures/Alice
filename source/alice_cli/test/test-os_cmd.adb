-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package body Test.OS_Cmd is

   procedure Run_OS_Cmd_And_Return_Success
     (Ctx : Alice.Context.Object; OS_Cmd : Alice.IFace.OS_Cmd.Object_Access) is
   begin
      Ctx.Log.Info ("Running OS command and expecting success");
      OS_Cmd.Run ("--version", Ctx);
   end Run_OS_Cmd_And_Return_Success;

   procedure Run_OS_Cmd_And_Return_Error
     (Ctx : Alice.Context.Object; OS_Cmd : Alice.IFace.OS_Cmd.Object_Access) is
   begin
      Ctx.Log.Info ("Running OS command and expecting error");
      OS_Cmd.Run ("--invalid-option", Ctx);
   end Run_OS_Cmd_And_Return_Error;

   procedure Timed_Run_Os_Cmd_And_Finish_In_Time
     (Ctx : Alice.Context.Object; OS_Cmd : Alice.IFace.OS_Cmd.Object_Access) is
   begin
      Ctx.Log.Info ("Running OS command with timeout and finishing in time");
      OS_Cmd.Timed_Run ("--version", Ctx, 5.0);
   end Timed_Run_Os_Cmd_And_Finish_In_Time;

   procedure Timed_Run_Os_Cmd_And_Timeout
     (Ctx : Alice.Context.Object; OS_Cmd : Alice.IFace.OS_Cmd.Object_Access) is
   begin
      Ctx.Log.Info ("Running OS command with timeout and expecting timeout");
      OS_Cmd.Timed_Run ("--sleep=10", Ctx, 5.0);
   end Timed_Run_Os_Cmd_And_Timeout;

end Test.OS_Cmd;
