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

with Alice.App.Query.Version;
with Alice.Result;

with Test.Activity;

procedure Alice_CLI is

   procedure Test_Activity is
      Activity : Alice.Log.Activity.CLI.Object_CLI;
   begin

      Test.Activity.Success (Activity, "Test number ONE ", 3);
      Alice.Log.Info ("Changing activity");
      delay 2.0;
      Test.Activity.Success (Activity, "Test number TWO ", 2);
      Alice.Log.Info ("Changing activity");
      delay 2.0;
      Test.Activity.Fatal_Error (Activity);
   end Test_Activity;
   pragma Unreferenced (Test_Activity);

begin
   --  Alice.Log.Optimize_For_CLI (False);  --  no color
   Alice.Log.Optimize_For_CLI (True);       -- colorized

   --  Alice.Log.Set_Verbose_Level (False);
   --  Alice.Log.Set_Verbose_Level (True);
   --  Alice.Log.Set_Trace_Level (False);
   --  Alice.Log.Set_Trace_Level (True);
   --  Alice.Log.Set_Debug_Level (False);
   Alice.Log.Set_Debug_Level (True);

   Alice.Log.Trace_Begin;

   --  Put_Line ("Welcome to the Alice " & Alice.Version & " CLI
   --  application!");

   declare
      Query_Version : Alice.App.Query.Version.Use_Case;
      Result        : constant Alice.Result.Object'Class := Query_Version.Run;
   begin
      case Result.Status is
         when Alice.Result.Success =>
            declare
               R : constant Alice.App.Query.Version.Result :=
                 Alice.App.Query.Version.Result (Result);
            begin
               Put_Line ("Alice version: " & R.Version'Image);
            end;

         when Alice.Result.Error =>
            --  #FIXME - Handle error properly with an Error_Handler object
            Alice.Log.Info
              ("Error retrieving Alice version: "
               & Alice.Str (Result.Message));
      end case;
   end;

   Alice.Log.Trace_End;

exception
   when E : others =>
      Alice.Log.Info ("Exception caught: " & Exception_Information (E));
end Alice_CLI;
