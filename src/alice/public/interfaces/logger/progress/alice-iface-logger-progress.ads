-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Alice.IFace.Logger.Progress is

   type Object is interface;
   --  The interface for the progress logger interface. It provides methods
   --  to start, stop, and log steps in an ongoing progress activity.

   type Object_Access is not null access all Object;
   --  The class type for the progress logger object access.

   procedure Start (Self : in out Object; Title : String) is abstract;
   --  Start a new progress logging activity with the given title.

   procedure Step (Self : in out Object; Message : String := "")
   is abstract;
   --  Log a step in the progress with the given message. Can be used to
   --  indicate progress or milestones in the activity. If the message is not
   --  provided, it defaults to an empty string.

   procedure Message (Self : in out Object; Message : String)
   is abstract;
   --  Write a message in the progress logging activity that is not
   --  specifically a step in the progress. Use it to provide additional
   --  information or context about the progress.

   procedure Stop (Self : in out Object) is abstract;
   --  Stop the current progress logging activity.

end Alice.IFace.Logger.Progress;
