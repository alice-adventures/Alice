-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Alice;
with Alice.Log;
with Alice.Log.Activity.CLI;

with Test.Activity;

procedure Alice_CLI is

begin
   --  Alice.Log.Optimize_For_CLI (False);  --  no color
   Alice.Log.Optimize_For_CLI (True);       -- colorized

   --  Alice.Log.Set_Verbose_Level (False);
   --  Alice.Log.Set_Verbose_Level (True);
   --  Alice.Log.Set_Trace_Level (False);
   --  Alice.Log.Set_Trace_Level (True);
   --  Alice.Log.Set_Debug_Level (False);
   Alice.Log.Set_Debug_Level (True);

   Alice.Log.Trace_Begin ("Alice_CLI");

   Put_Line ("Welcome to the Alice " & Alice.Version & " CLI application!");

   declare
      Activity : Alice.Log.Activity.CLI.Object_CLI;
   begin
      Test.Activity.Success (Activity, "Test number ONE ", 3);
      Alice.Log.Warning ("Changing activity");
      delay 2.0;
      Test.Activity.Success (Activity, "Test number TWO ", 2);
      Alice.Log.Warning ("Changing activity");
      delay 2.0;
      Test.Activity.Fatal_Error (Activity);
   end;

   Alice.Log.Trace_End ("Alice_CLI");

exception
   when E : others =>
      Alice.Log.Error ("Exception caught: " & Exception_Information (E));
end Alice_CLI;
