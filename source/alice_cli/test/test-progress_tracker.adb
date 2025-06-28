-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Exceptions;
with GNAT.Source_Info;

package body Test.Progress_Tracker is

   -------------------------------
   -- Activity_With_No_Messages --
   -------------------------------

   procedure Activity_With_No_Messages
     (Log      : Alice.IFace.Logger.Object_Access;
      Progress : Alice.IFace.Progress_Tracker.Object_Access;
      Length   : Integer)
   is
      Title : constant String := "Activity with no messages: ";
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);
      Log.Trace_Begin;

      Progress.Start (Title);
      for I in 1 .. Length loop
         Progress.Step
           (Title & Integer'Image (I) & " of " & Integer'Image (Length) & " ");

         delay 0.1;  --  doing things ...

      end loop;
      Progress.Stop;
      Log.Info ("Activity completed successfully");

      Log.Trace_End;
   end Activity_With_No_Messages;

   ----------------------------
   -- Activity_With_Messages --
   ----------------------------

   procedure Activity_With_Messages
     (Log      : Alice.IFace.Logger.Object_Access;
      Progress : Alice.IFace.Progress_Tracker.Object_Access;
      Length   : Integer)
   is
      Title : constant String := "Activity with messages: ";
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);
      Log.Trace_Begin;

      Progress.Start (Title);
      for I in 1 .. Length loop
         Progress.Step
           (Title & Integer'Image (I) & " of " & Integer'Image (Length) & " ");

         delay 0.1;  --  doing things ...

         Progress.Message ("This is a message for step " & Integer'Image (I));
         Log.Info ("This is a verbose message for step " & Integer'Image (I));
      end loop;
      Progress.Stop;
      Log.Info ("Activity completed successfully");

      Log.Trace_End;
   end Activity_With_Messages;

   ------------------------------
   -- Bug_That_Throw_Exception --
   ------------------------------

   procedure Bug_That_Throw_Exception
     (Progress : Alice.IFace.Progress_Tracker.Object_Access) is
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);
      --  Using the Activity without previously calling Activity.Start
      --  produces a fatal error.
      Progress.Step ("Call to Step with no previous call to Start");
   end Bug_That_Throw_Exception;

   ---------
   -- Run --
   ---------

   procedure Run
     (Log      : Alice.IFace.Logger.Object_Access;
      Progress : Alice.IFace.Progress_Tracker.Object_Access) is
   begin
      Activity_With_No_Messages (Log, Progress, 5);
      Activity_With_Messages (Log, Progress, 3);

      Bug_That_Throw_Exception (Progress);
   exception
      when E : others =>
         Log.Info
           ("Exception caught: " & Ada.Exceptions.Exception_Information (E));
   end Run;

end Test.Progress_Tracker;
