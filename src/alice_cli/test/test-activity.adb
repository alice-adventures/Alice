-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.Log;

package body Test.Activity is

   -------------
   -- Success --
   -------------

   procedure Success
     (Activity : in out Alice.Log.Activity.Object'Class;
      Title    : String;
      Length   : Integer) is
   begin
      Alice.Log.Trace_Begin ("Activity Test: " & Title);

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

      Alice.Log.Trace_End ("Activity Test: " & Title);
   end Success;

   -----------------
   -- Fatal_Error --
   -----------------

   procedure Fatal_Error (Activity : in out Alice.Log.Activity.Object'Class) is
   begin
      --  Using the Activity without previously calling Activity.Start
      --  produces a fatal error.
      Activity.Step ("Call to Step with no previous call to Start");
   end Fatal_Error;

end Test.Activity;
