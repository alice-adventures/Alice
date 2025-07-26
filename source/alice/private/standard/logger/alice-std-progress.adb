-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

with Alice.Env;

package body Alice.Std.Progress is

   use all type Simple_Logging.Levels;

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
      Self.Message := Alice.UStr (Title);
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
         Self.Message := Alice.UStr (Message);
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
         if Simple_Logging.Level >= Simple_Logging.Info then
            Simple_Logging.Always ("o " & Alice.Str (Self.Message));
         end if;
      end if;
      Self.Ongoing := null;
   end Stop;

   --------------------
   -- Progress_Image --
   --------------------

   procedure Put_Image_Progress
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Self   : Object) is
   begin
      Output.Put ("([" & Self'Address'Image & " ] with");
      Alice.Env.Increase_Indent (Output);

      Output.New_Line;
      if Self.Ongoing /= null then
         Output.Put ("Ongoing => ");
         Output.Increase_Indent;
         Output.Put (Self.Ongoing.all'Image);
         Output.Decrease_Indent;
      else
         Output.Put ("Ongoing => null");
      end if;
      Output.New_Line;

      Alice.Env.Decrease_Indent (Output);
      Output.Put (")");
   end Put_Image_Progress;

end Alice.Std.Progress;
