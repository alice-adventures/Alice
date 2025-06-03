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

   -----------------
   -- Fatal_Error --
   -----------------

   procedure Fatal_Error is
   begin
      raise Program_Error
        with "Activity not started, call Start before using Step or Message";
   end Fatal_Error;

   -----------
   -- Start --
   -----------

   overriding
   procedure Start (This : in out Object_CLI; Title : String) is
   begin
      This.Ongoing :=
        new Simple_Logging.Ongoing'
          (Simple_Logging.Activity (Title, Simple_Logging.Warning));
   end Start;

   ----------
   -- Step --
   ----------

   overriding
   procedure Step (This : in out Object_CLI; Message : String := "") is
   begin
      if This.Ongoing = null then
         Fatal_Error;
      else
         Simple_Logging.Step (This.Ongoing.all, Message);
      end if;
   end Step;

   -------------
   -- Message --
   -------------

   overriding
   procedure Message (This : in out Object_CLI; Message : String) is
   begin
      if This.Ongoing = null then
         Fatal_Error;
      else
         Simple_Logging.Always (Message);
      end if;
   end Message;

   ----------
   -- Stop --
   ----------

   overriding
   procedure Stop (This : in out Object_CLI) is
   begin
      if This.Ongoing = null then
         Fatal_Error;
      else
         Free_Ongoing (This.Ongoing);
      end if;
      This.Ongoing := null;
   end Stop;

end Alice.Log.Activity.CLI;
