-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body Alice.Std.Log.Progress is

   ------------------
   -- Free_Ongoing --
   ------------------

   procedure Free_Ongoing is new
     Ada.Unchecked_Deallocation (Simple_Logging.Ongoing'Class, Ongoing_Access);

   ---------
   -- Bug --
   ---------

   procedure Bug is
   begin
      raise Program_Error
        with "Activity not started, call Start before using other methods";
   end Bug;

   -----------
   -- Start --
   -----------

   overriding
   procedure Start (Self : in out Object; Title : String) is
   begin
      Self.Ongoing :=
        new Simple_Logging.Ongoing'
          (Simple_Logging.Activity (Title, Simple_Logging.Warning));
   end Start;

   ----------
   -- Step --
   ----------

   overriding
   procedure Step (Self : in out Object; Message : String := "") is
   begin
      if Self.Ongoing = null then
         Bug;
      else
         Simple_Logging.Step (Self.Ongoing.all, Message);
      end if;
   end Step;

   -------------
   -- Message --
   -------------

   overriding
   procedure Message (Self : in out Object; Message : String) is
   begin
      if Self.Ongoing = null then
         Bug;
      else
         Simple_Logging.Always (Message);
      end if;
   end Message;

   ----------
   -- Stop --
   ----------

   overriding
   procedure Stop (Self : in out Object) is
   begin
      if Self.Ongoing = null then
         Bug;
      else
         Free_Ongoing (Self.Ongoing);
      end if;
      Self.Ongoing := null;
   end Stop;

end Alice.Std.Log.Progress;
