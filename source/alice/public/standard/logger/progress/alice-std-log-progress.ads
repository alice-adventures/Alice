-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package provides the standard implementation of the Alice Progress
--  Logger interface, which  is enough for the CLI and other simple
--  applications.

with Alice.IFace.Logger.Progress;

with Simple_Logging;

package Alice.Std.Log.Progress is

   type Object is new Alice.IFace.Logger.Progress.Object with private;

   overriding
   procedure Start (Self : in out Object; Title : String);
   --  Starts a new ongoing activity with the given title.

   overriding
   procedure Step (Self : in out Object; Message : String := "");
   --  Logs a step in the ongoing activity with an optional message. If the
   --  message is not provided, it defaults to an empty string. This can be
   --  used to indicate progress or milestones in the activity.

   overriding
   procedure Message (Self : in out Object; Message : String);
   --  Write a message in the ongoing activity that is not specifically a step
   --  in the activity. Use it to provide additional information or context
   --  about the activity. Do not use regular output, like
   --  Ada.Text_IO.Put_Line; instead, use the logging system or this
   --  procedure.

   overriding
   procedure Stop (Self : in out Object);
   --  Stops the ongoing activity.

private

   type Ongoing_Access is access all Simple_Logging.Ongoing'Class;

   type Object is new Alice.IFace.Logger.Progress.Object with record
      Ongoing : Ongoing_Access := null;
   end record;

end Alice.Std.Log.Progress;
