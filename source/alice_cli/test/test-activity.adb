-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package body Test.Activity is

   ------------------
   -- With_Success --
   ------------------

   procedure With_Success
     (Ctx : Alice.Context.Object; Title : String; Length : Integer) is
   begin
      Ctx.Log.Trace_Begin ("Activity Test: " & Title);

      Ctx.Progress.Start (Title);
      for I in 1 .. Length loop
         Ctx.Progress.Step
           (Title & Integer'Image (I) & " of " & Integer'Image (Length) & " ");

         delay 1.0;  --  doing things ...

         Ctx.Progress.Message
           ("This is a message for step " & Integer'Image (I));
         Ctx.Log.Info
           ("This is a verbose message for step " & Integer'Image (I));
      end loop;
      Ctx.Progress.Stop;

      Ctx.Log.Debug ("Seen Finalize?");
      delay 1.0;

      Ctx.Log.Trace_End ("Activity Test: " & Title);
   end With_Success;

   ----------------
   -- With_Error --
   ----------------

   procedure With_Error
     (Ctx : Alice.Context.Object; Title : String; Length : Integer) is
   begin
      null;
   end With_Error;

   --------------------
   -- With_Exception --
   --------------------

   procedure With_Exception (Ctx : Alice.Context.Object) is
   begin
      --  Using the Activity without previously calling Activity.Start
      --  produces a fatal error.
      Ctx.Progress.Step ("Call to Step with no previous call to Start");
   end With_Exception;

end Test.Activity;
