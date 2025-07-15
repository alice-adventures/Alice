-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Exceptions;
with GNAT.Source_Info;

package body Test.Progress is

   -------------------------------
   -- Activity_With_No_Messages --
   -------------------------------

   procedure Activity_With_No_Messages
     (Progress : Alice.IFace.Progress_Tracker.Object_Access;
      Length   : Integer)
   is
      Title : constant String := "Activity with no messages: ";
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);

      Progress.Start (Title);
      for I in 1 .. Length loop
         Progress.Step
           (Title & Integer'Image (I) & " of " & Integer'Image (Length) & " ");

         delay 0.01;  --  doing things ...

      end loop;
      Progress.Stop;

      Test.Pass;

   exception
      when E : others =>
         Test.Fail
           ("Exception caught: " & Ada.Exceptions.Exception_Information (E));
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

      Progress.Start (Title);
      for I in 1 .. Length loop
         Progress.Step
           (Title & Integer'Image (I) & " of " & Integer'Image (Length) & " ");

         delay 0.01;  --  doing things ...

         Progress.Message ("This is a message for step " & Integer'Image (I));
         Log.Info ("This is a verbose message for step " & Integer'Image (I));
      end loop;
      Progress.Stop;

      Test.Pass;

   exception
      when E : others =>
         Test.Fail
           ("Exception caught: " & Ada.Exceptions.Exception_Information (E));
   end Activity_With_Messages;

   ------------------------------
   -- Bug_That_Throw_Exception --
   ------------------------------

   procedure Bug_That_Throw_Exception
     (Log      : Alice.IFace.Logger.Object_Access;
      Progress : Alice.IFace.Progress_Tracker.Object_Access) is
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);
      --  Using the Activity without previously calling Activity.Start
      --  produces a fatal error.
      Progress.Step ("Call to Step with no previous call to Start");

   exception
      when E : Program_Error =>
         Log.Warning
           ("Exception caught: " & Ada.Exceptions.Exception_Information (E));
         Test.Pass;

      when E : others =>
         Test.Fail
           ("Exception caught: " & Ada.Exceptions.Exception_Information (E));
   end Bug_That_Throw_Exception;

   ---------
   -- Run --
   ---------

   procedure Run
     (Log      : Alice.IFace.Logger.Object_Access;
      Progress : Alice.IFace.Progress_Tracker.Object_Access) is
   begin
      Activity_With_No_Messages (Progress, 5);
      Activity_With_Messages (Log, Progress, 3);
      Bug_That_Throw_Exception (Log, Progress);
   end Run;

end Test.Progress;
