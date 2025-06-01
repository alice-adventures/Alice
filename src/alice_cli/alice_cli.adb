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
with Alice.Log.Activity;
with Alice.Log.Activity.CLI;

procedure Alice_CLI is

   procedure Activity_Test
     (Activity : in out Alice.Log.Activity.Object'Class;
      Title    : String;
      Length   : Integer) is
   begin
      Alice.Log.Trace_Begin ("Activity Test " & Title);

      Activity.Start (Title);
      for I in 1 .. Length loop
         Activity.Step
           (Title & Integer'Image (I) & " of " & Integer'Image (Length) & " ");

         delay 1.0;  --  doing things ...

         Activity.Message ("This is a message for step " & Integer'Image (I));
         Alice.Log.Info
           ("This is a verbose message for step " & Integer'Image (I));
      end loop;
      Activity.Stop;

      Alice.Log.Debug ("Seen Finalize?");
      delay 1.0;

      Alice.Log.Trace_End ("Activity Test " & Title);
   end Activity_Test;

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
      Activity_Test (Activity, "Test number ONE ", 3);
      Alice.Log.Warning ("Changing activity");
      delay 2.0;
      Activity_Test (Activity, "Test number TWO ", 2);
   end;

   Alice.Log.Trace_End ("Alice_CLI");

exception
   when E : others =>
      Alice.Log.Error ("Exception caught: " & Exception_Information (E));
end Alice_CLI;
