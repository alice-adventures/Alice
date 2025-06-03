-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Alice.Log.Activity is

   type Object is limited interface;
   --  This is the interface for the activity logging. It provides methods to
   --  start, stop, and log steps in an ongoing activity.

   procedure Start (This : in out Object; Title : String)
   is abstract;
   --  Starts a new ongoing activity with the given title.

   procedure Step (This : in out Object; Message : String := "") is abstract;
   --  Logs a step in the ongoing activity with an optional message.
   --  If the message is not provided, it defaults to an empty string.
   --  This can be used to indicate progress or milestones in the activity.

   procedure Message (This : in out Object; Message : String) is abstract;
   --  Writes a message in the ongoing activity that is not specifically a
   --  step in the activity. Use it to provide additional information or
   --  context about the activity. Do not use regular output, like
   --  Ada.Text_IO.Put_Line; instead, use the logging system or this
   --  procedure.

   procedure Stop (This : in out Object) is abstract;
   --  Stops the ongoing activity.

end Alice.Log.Activity;
