-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO;

with Alice;
--  with Alice.App.Query.Version;
with Alice.Context;
with Alice.IFace.OS_Cmd;
with Alice.OS_Context;
with Alice.Result;
with Alice.Std;
--  with Alice.Std.Error_Handler;
--  with Alice.Std.Log;
--  with Alice.Std.Progress;
--  with Alice.Std.OS_Cmd;

procedure Alice_CLI is

   OS_Ctx : constant Alice.OS_Context.Object := Alice.Std.Get_OS_Context;
   Ctx    : constant Alice.Context.Object := Alice.Std.Get_Context;

begin
   --  SELECT LOG LEVEL -------------------------------------------------------
   --  Ctx.Log.Optimize_For_CLI (With_Color_Enabled => False);
   Ctx.Log.Optimize_For_CLI (With_Color_Enabled => True);

   --  Ctx.Log.SetDefault_Level;
   Ctx.Log.Set_Verbose_Level;
   --  Ctx.Log.Set_Trace_Level (With_Location_Enabled => False);
   --  Ctx.Log.Set_Trace_Level (With_Location_Enabled => True);
   --  Ctx.Log.Set_Debug_Level (With_Location_Enabled => False);
   --  Ctx.Log.Set_Debug_Level (With_Location_Enabled => True);
   --  -------------------------------------------------------------------------

   Ada.Text_IO.Put_Line ("Welcome to the Alice CLI application!");

   Ctx.Log.Trace_End;
end Alice_CLI;
