-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body Alice.Log.Activity.CLI is

   procedure Free_Ongoing is new
     Ada.Unchecked_Deallocation (Simple_Logging.Ongoing'Class, Ongoing_Access);

   overriding
   procedure Start (This : in out Object_CLI; Title : String) is
   begin
      This.Ongoing :=
        new Simple_Logging.Ongoing'
          (Simple_Logging.Activity (Title, Simple_Logging.Warning));
   end Start;

   overriding
   procedure Step (This : in out Object_CLI; Message : String := "") is
   begin
      if This.Ongoing /= null then
         Simple_Logging.Step (This.Ongoing.all, Message);
      else
         Alice.Log.Error ("Ongoing activity is not started.", -1);
      end if;
   end Step;

   overriding
   procedure Message (This : in out Object_CLI; Message : String) is
   begin
      if This.Ongoing /= null then
         Simple_Logging.Always (Message);
      else
         Alice.Log.Error ("Ongoing activity is not started.", -1);
      end if;
   end Message;

   overriding
   procedure Stop (This : in out Object_CLI) is
   begin
      Free_Ongoing (This.Ongoing);
      This.Ongoing := null;
   end Stop;

end Alice.Log.Activity.CLI;
