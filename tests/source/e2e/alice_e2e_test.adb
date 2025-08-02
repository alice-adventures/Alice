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
--  with Alice.App.Query.Version;
with Alice.Context;
with Alice.Std;

with Test;
with Test.Query.Version;

procedure Alice_E2E_Test is

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
     (" --                   ALICE  END TO END  TESTS                     --");
   Put_Line
     (" --                                                                --");
   Put_Line
     (" --------------------------------------------------------------------");
   Put (ANSI.Reset);

   Test.Section ("USE CASE QUERIES", ANSI.Yellow);
   Test.Query.Version.Run;

   Test.Summary;

   Context.Log.Set_Trace_Level (With_Location_Enabled => False);
   New_Line;
   Context.Log.Trace ("Finalization of Controlled Objects");
   New_Line;

end Alice_E2E_Test;
