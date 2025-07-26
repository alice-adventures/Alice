-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.Env;

package body Alice.OS_Context is

   ----------------------
   -- OS_Context_Image --
   ----------------------

   procedure Put_Image_OS_Context
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Self   : Object) is
   begin
      Output.Put ("([" & Self'Address'Image & " ] with");
      Alice.Env.Increase_Indent (Output);

      Output.New_Line;
      Output.Put ("Err => " & Self.Err.all'Image);
      Output.New_Line;
      Output.Put ("Log => " & Self.Log.all'Image);
      Output.New_Line;

      Alice.Env.Decrease_Indent (Output);
      Output.Put (")");
   end Put_Image_OS_Context;

end Alice.OS_Context;
