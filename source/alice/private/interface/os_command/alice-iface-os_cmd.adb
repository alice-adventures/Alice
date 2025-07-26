-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.Env;

package body Alice.IFace.OS_Cmd is

   -----------------------------
   -- Put_Image_Output_Result --
   -----------------------------

   procedure Put_Image_Output_Result
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Self   : Output_Result) is
   begin
      Output.Put ("([" & Self'Address'Image & " ] with");
      Alice.Env.Increase_Indent (Output);

      Output.New_Line;
      Output.Put ("Status  => " & Self.Status'Image);
      Output.New_Line;

      case Self.Status is
         when Alice.Result.Success =>
            null;

         when Alice.Result.Error =>
            Output.Put ("Level   => " & Self.Level'Image);
            Output.New_Line;
            Output.Put ("Message => " & Self.Message'Image);
            Output.New_Line;
            Output.Put ("Hint    => " & Self.Hint'Image);
            Output.New_Line;
      end case;

      Output.Put ("Exit_Status => " & Self.Exit_Status'Image);
      Output.New_Line;
      Output.Put ("Temp_FD     => " & Self.Temp_FD'Image);
      Output.New_Line;
      Output.Put ("Temp_File   => " & Self.Temp_File.all'Image);
      Output.New_Line;

      Alice.Env.Decrease_Indent (Output);
      Output.Put (")");
   end Put_Image_Output_Result;

end Alice.IFace.OS_Cmd;
