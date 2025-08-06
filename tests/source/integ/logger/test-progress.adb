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

   package Activity is
      procedure Without_Initialization_Throws_Exception
        (Log      : Alice.IFace.Logger.Object_Access;
         Progress : Alice.IFace.Progress_Tracker.Object_Access);

      procedure Without_Messages
        (Progress : Alice.IFace.Progress_Tracker.Object_Access;
         Length   : Integer);

      procedure With_Messages
        (Log      : Alice.IFace.Logger.Object_Access;
         Progress : Alice.IFace.Progress_Tracker.Object_Access;
         Length   : Integer);
   end Activity;

   package body Activity is

      ---------------------------------------------
      -- Without_Initialization_Throws_Exception --
      ---------------------------------------------

      procedure Without_Initialization_Throws_Exception
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
              ("Exception caught: "
               & Ada.Exceptions.Exception_Information (E));
            Test.Pass;

         when E : others =>
            Test.Fail
              ("Exception caught: "
               & Ada.Exceptions.Exception_Information (E));
      end Without_Initialization_Throws_Exception;

      ----------------------
      -- Without_Messages --
      ----------------------

      procedure Without_Messages
        (Progress : Alice.IFace.Progress_Tracker.Object_Access;
         Length   : Integer)
      is
         Title : constant String := "Activity with no messages: ";
      begin
         Test.Title (GNAT.Source_Info.Enclosing_Entity);

         Test.Subtitle ("Activity that ends successfully");
         Progress.Start (Title);
         for I in 1 .. Length loop
            Progress.Step
              (Title & Integer'Image (I) & " of " & Integer'Image (Length));
            delay 0.01;  --  doing things ...
         end loop;
         Progress.Done;

         Test.Subtitle ("Activity that fails");
         Progress.Start (Title);
         for I in 1 .. Length loop
            Progress.Step
              (Title & Integer'Image (I) & " of " & Integer'Image (Length));
            delay 0.01;  --  doing things ...
         end loop;
         Progress.Fail;

         Test.Pass;

      exception
         when E : others =>
            Test.Fail
              ("Exception caught: "
               & Ada.Exceptions.Exception_Information (E));
      end Without_Messages;

      -------------------
      -- With_Messages --
      -------------------

      procedure With_Messages
        (Log      : Alice.IFace.Logger.Object_Access;
         Progress : Alice.IFace.Progress_Tracker.Object_Access;
         Length   : Integer)
      is
         Title : constant String := "Activity with messages: ";
      begin
         Test.Title (GNAT.Source_Info.Enclosing_Entity);

         Test.Subtitle ("Activity that ends successfully with messages");
         Progress.Start (Title);
         for I in 1 .. Length loop
            Progress.Step
              (Title & Integer'Image (I) & " of " & Integer'Image (Length));
            delay 0.01;  --  doing things ...
            Progress.Message
              ("This is a message for step " & Integer'Image (I));
            Log.Info
              ("This is a verbose message for step " & Integer'Image (I));
         end loop;
         Progress.Done;

         Test.Subtitle ("Activity that fails with messages");
         Progress.Start (Title);
         for I in 1 .. Length loop
            Progress.Step
              (Title & Integer'Image (I) & " of " & Integer'Image (Length));
            delay 0.01;  --  doing things ...
            Progress.Message
              ("This is a message for step " & Integer'Image (I));
            Log.Info
              ("This is a verbose message for step " & Integer'Image (I));
         end loop;
         Progress.Fail;

         Test.Pass;

      exception
         when E : others =>
            Test.Fail
              ("Exception caught: "
               & Ada.Exceptions.Exception_Information (E));
      end With_Messages;

   end Activity;

   -------------------
   -- Run_All_Tests --
   -------------------

   procedure Run_All_Tests
     (Log      : Alice.IFace.Logger.Object_Access;
      Progress : Alice.IFace.Progress_Tracker.Object_Access) is
   begin
      Activity.Without_Initialization_Throws_Exception (Log, Progress);
      Activity.Without_Messages (Progress, 5);
      Activity.With_Messages (Log, Progress, 3);
   end Run_All_Tests;

end Test.Progress;
