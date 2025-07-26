-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.Env;

package body Alice.Result is

   ------------------
   -- Create_Error --
   ------------------

   function Create_Error
     (Level : Error_Level; Message : Alice.UString) return Error_Object'Class
   is
   begin
      return
         Result : constant Error_Object :=
           (Alice.Controlled
            with
              Status  => Alice.Result.Error,
              Level   => Level,
              Message => Message,
              Hint    => Alice.Hint.None);
   end Create_Error;

   ----------------------
   -- Put_Image_Result --
   ----------------------

   procedure Put_Image_Result
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Self   : Object) is
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

      Alice.Env.Decrease_Indent (Output);
      Output.Put (")");
   end Put_Image_Result;

end Alice.Result;
