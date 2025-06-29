-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.Context;
with Alice.IFace.OS_Cmd;

package Test.OS_Cmd is

   procedure Run_OS_Cmd_And_Return_Success
     (Ctx : Alice.Context.Object; OS_Cmd : Alice.IFace.OS_Cmd.Object_Access);

   procedure Run_OS_Cmd_And_Return_Error
     (Ctx : Alice.Context.Object; OS_Cmd : Alice.IFace.OS_Cmd.Object_Access);

   procedure Timed_Run_Os_Cmd_And_Finish_In_Time
     (Ctx : Alice.Context.Object; OS_Cmd : Alice.IFace.OS_Cmd.Object_Access);

   procedure Timed_Run_Os_Cmd_And_Timeout
     (Ctx : Alice.Context.Object; OS_Cmd : Alice.IFace.OS_Cmd.Object_Access);

end Test.OS_Cmd;
