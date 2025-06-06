-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package specifies the interface for a progress logger in the Alice
--  application. It provides methods to start, stop, and log steps in an
--  ongoing progress activity. The interface is designed to be implemented by
--  different logging backends, allowing for flexibility in how progress is
--  reported. The `Start` method initializes a new progress activity with a
--  title, while the `Step` method logs a step in the progress with an
--  optional message. The `Message` method allows for additional context to be
--  logged, and the `Stop` method concludes the progress activity. This
--  interface is useful for tracking the progress of long-running operations,
--  providing feedback to users, and integrating with various logging systems.
--  The interface is designed to be used in conjunction with the Alice
--  application context, which provides the necessary logging infrastructure.

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
