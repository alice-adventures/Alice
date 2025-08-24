-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.Env;

package body Alice.OS_Commands is

   ----------------------
   -- Put_Image_OS_Cmd --
   ----------------------

   procedure Put_Image_OS_Cmd
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Self   : Object) is
   begin
      Output.Put ("([" & Self'Address'Image & " ] with");
      Alice.Env.Increase_Indent (Output);

      Output.New_Line;
      Output.Put ("Alr  => " & Self.Alr.all'Image);
      Output.New_Line;
      Output.Put ("Curl => " & Self.Curl.all'Image);
      Output.New_Line;
      Output.Put ("Git  => " & Self.Git.all'Image);

      Alice.Env.Decrease_Indent (Output);
      Output.Put (")");
   end Put_Image_OS_Cmd;

end Alice.OS_Commands;
