-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.Env;

package body Alice.Context is

   ------------------
   -- OS_Cmd_Image --
   ------------------

   procedure OS_Cmd_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Self   : OS_Commands) is
   begin
      Output.Put ("([" & Self'Address'Image & " ] with");
      Alice.Env.Increase_Indent (Output);

      Output.New_Line;
      Output.Put ("Alr  => " & Self.Alr.all'Image);
      Output.New_Line;
      Output.Put ("Curl => " & Self.Curl.all'Image);
      Output.New_Line;
      Output.Put ("Git  => " & Self.Git.all'Image);
      Output.New_Line;

      Alice.Env.Decrease_Indent (Output);
      Output.Put (")");
   end OS_Cmd_Image;

   -------------------
   -- Context_Image --
   -------------------

   procedure Context_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Self   : Object) is
   begin
      Output.Put ("([" & Self'Address'Image & " ] with");
      Alice.Env.Increase_Indent (Output);

      Output.New_Line;
      Output.Put ("Err" & "    => " & Self.Err.all'Image);
      Output.New_Line;
      Output.Put ("Log" & "    => " & Self.Log.all'Image);
      Output.New_Line;
      Output.Put ("Prog" & "   => " & Self.Prog.all'Image);
      Output.New_Line;
      Output.Put ("OS_Cmd" & " => " & Self.OS_Cmd'Image);
      Output.New_Line;

      Alice.Env.Decrease_Indent (Output);
      Output.Put (")");
   end Context_Image;

end Alice.Context;
