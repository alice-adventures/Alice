-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

with AnsiAda;

with Alice;
with Alice.Context;
with Alice.Std;

with Test;
with Test.Log;
with Test.OS_Cmd;
with Test.Progress;
with Test.VCS.Profile;
with Test.VCS.Service;

procedure Alice_Integ_Test is

   package ANSI renames AnsiAda;

   Context : constant Alice.Context.Object_Access := Alice.Std.Get_Context;

begin
   --  SELECT LOG LEVEL -------------------------------------------------------
   --  Context.Log.Optimize_For_CLI (With_Color_Enabled => False);
   Context.Log.Optimize_For_CLI (With_Color_Enabled => True);

   --  Context.Log.SetDefault_Level;
   Context.Log.Set_Verbose_Level;
   --  Context.Log.Set_Trace_Level (With_Location_Enabled => False);
   --  Context.Log.Set_Trace_Level (With_Location_Enabled => True);
   --  Context.Log.Set_Debug_Level (With_Location_Enabled => False);
   --  Context.Log.Set_Debug_Level (With_Location_Enabled => True);
   --  ------------------------------------------------------------------------

   Put (ANSI.Reset_All);
   Put (ANSI.Foreground (ANSI.Green));
   --  Put_Line (ANSI.Clear_To_Beginning_Of_Screen);
   Put_Line
     (" --------------------------------------------------------------------");
   Put_Line
     (" --                                                                --");
   Put_Line
     (" --                  ALICE  INTEGRATION  TESTS                     --");
   Put_Line
     (" --                                                                --");
   Put_Line
     (" --------------------------------------------------------------------");
   Put (ANSI.Reset);

   Test.Section ("SYSTEM COMPONENTS", ANSI.Light_Cyan);

   Context.Log.Save_State;
   Test.Log.Run (Context.Log);
   Context.Log.Restore_State;

   Test.Progress.Run (Context.Log, Context.Progress);
   Test.OS_Cmd.Run (Context);

   Test.Section ("VCS COMPONENTS", ANSI.Light_Cyan);

   Context.Log.Save_State;
   Context.Log.Set_Debug_Level (With_Location_Enabled => True);
   Test.VCS.Profile.Run (Context);
   Test.VCS.Service.Run (Context);
   Context.Log.Restore_State;

   Test.Summary;

   Context.Log.Set_Trace_Level (With_Location_Enabled => False);
   New_Line;
   Context.Log.Trace ("Finalization of Controlled Objects");
   New_Line;
end Alice_Integ_Test;
