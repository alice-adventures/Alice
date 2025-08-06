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
with Test.VCS.Service.GitHub;

procedure Alice_Integ_Test is

   package ANSI renames AnsiAda;

   Context : constant Alice.Context.Object_Access := Alice.Std.Get_Context;

   Main_Color : constant ANSI.Colors := ANSI.Light_Cyan;

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

   begin
      Test.Section ("Alice.IFace.Logger", Main_Color);
      Context.Log.Save_State;
      Test.Log.Run_All_Tests (Context.Log);
      Context.Log.Restore_State;

      Test.Section ("Alice.IFace.Progress", Main_Color);
      Test.Progress.Run_All_Tests (Context.Log, Context.Progress);
   end;

   begin
      Test.Section ("Alice.IFace.OS_Cmd", Main_Color);
      Test.OS_Cmd.Run_All_Tests (Context);
   end;

   begin
      Test.Section ("Alice.VCS.Profile", Main_Color);

      Context.Log.Save_State;
      Context.Log.Set_Debug_Level (With_Location_Enabled => True);

      Test.VCS.Profile.Run_All_Tests (Context);

      Test.Section ("Alice.VCS.Service.GitHub", Main_Color);
      Test.VCS.Service.GitHub.Run_All_Tests (Context);

      Context.Log.Restore_State;
   end;

   Test.Summary;

   Context.Log.Set_Trace_Level (With_Location_Enabled => False);
   New_Line;
   Context.Log.Trace ("Finalization of Controlled Objects");
   New_Line;
end Alice_Integ_Test;
