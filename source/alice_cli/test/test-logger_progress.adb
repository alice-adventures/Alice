-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Exceptions;
with GNAT.Source_Info;

package body Test.Logger_Progress is

   -------------------------------
   -- Activity_With_No_Messages --
   -------------------------------

   procedure Activity_With_No_Messages
     (Ctx : Alice.Context.Object; Length : Integer)
   is
      Title : constant String := "Activity with no messages: ";
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);
      Ctx.Log.Trace_Begin;

      Ctx.Progress.Start (Title);
      for I in 1 .. Length loop
         Ctx.Progress.Step
           (Title & Integer'Image (I) & " of " & Integer'Image (Length) & " ");

         delay 0.1;  --  doing things ...

      end loop;
      Ctx.Progress.Stop;
      Ctx.Log.Info ("Activity completed successfully");

      Ctx.Log.Trace_End;
   end Activity_With_No_Messages;

   ----------------------------
   -- Activity_With_Messages --
   ----------------------------

   procedure Activity_With_Messages
     (Ctx : Alice.Context.Object; Length : Integer)
   is
      Title : constant String := "Activity with messages: ";
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);
      Ctx.Log.Trace_Begin;

      Ctx.Progress.Start (Title);
      for I in 1 .. Length loop
         Ctx.Progress.Step
           (Title & Integer'Image (I) & " of " & Integer'Image (Length) & " ");

         delay 0.1;  --  doing things ...

         Ctx.Progress.Message
           ("This is a message for step " & Integer'Image (I));
         Ctx.Log.Info
           ("This is a verbose message for step " & Integer'Image (I));
      end loop;
      Ctx.Progress.Stop;
      Ctx.Log.Info ("Activity completed successfully");

      Ctx.Log.Trace_End;
   end Activity_With_Messages;

   ------------------------------
   -- Bug_That_Throw_Exception --
   ------------------------------

   procedure Bug_That_Throw_Exception (Ctx : Alice.Context.Object) is
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);
      --  Using the Activity without previously calling Activity.Start
      --  produces a fatal error.
      Ctx.Progress.Step ("Call to Step with no previous call to Start");
   end Bug_That_Throw_Exception;

   ---------
   -- Run --
   ---------

   procedure Run (Ctx : Alice.Context.Object) is
   begin
      Activity_With_No_Messages (Ctx, 5);
      Activity_With_Messages (Ctx, 3);

      Bug_That_Throw_Exception (Ctx);
   exception
      when E : others =>
         Ctx.Log.Info
           ("Exception caught: " & Ada.Exceptions.Exception_Information (E));
   end Run;

end Test.Logger_Progress;
