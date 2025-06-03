-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Simple_Logging;

package Alice.Log.Activity.CLI is

   type Object_CLI is limited new Object with private;

   overriding
   procedure Start (This : in out Object_CLI; Title : String);

   overriding
   procedure Step (This : in out Object_CLI; Message : String := "");

   overriding
   procedure Message (This : in out Object_CLI; Message : String);

   overriding
   procedure Stop (This : in out Object_CLI);

private

   type Ongoing_Access is access all Simple_Logging.Ongoing'Class;

   type Object_CLI is limited new Object with record
      Ongoing : Ongoing_Access := null;
   end record;

end Alice.Log.Activity.CLI;
