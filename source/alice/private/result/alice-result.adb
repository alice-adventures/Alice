-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.Env;

package body Alice.Result is

   -------------
   -- Success --
   -------------

   function Success return Object'Class is
   begin
      return
         Result : constant Object :=
           (Alice.Controlled with Status => Alice.Result.Success);
   end Success;

   -----------------------
   -- Success_With_Data --
   -----------------------

   function Success_With_Data
     (Value : Alice.UString) return Object_With_Data'Class is
   begin
      return
         Result : constant Object_With_Data :=
           (Alice.Controlled
            with Status => Alice.Result.Success, Data => Value);
   end Success_With_Data;

   -----------
   -- Error --
   -----------

   function Error
     (Level   : Error_Level;
      Message : Alice.UString;
      Hint    : Alice.Hint.Id := Alice.Hint.None) return Object'Class is
   begin
      return
         Result : constant Object :=
           (Alice.Controlled
            with
              Status  => Alice.Result.Error,
              Level   => Level,
              Message => Message,
              Hint    => Hint);
   end Error;

   ---------------------
   -- Error_With_Data --
   ---------------------

   function Error_With_Data
     (Level   : Error_Level;
      Message : Alice.UString;
      Hint    : Alice.Hint.Id := Alice.Hint.None) return Object_With_Data'Class
   is
   begin
      return
         Result : constant Object_With_Data :=
           (Alice.Controlled
            with
              Status  => Alice.Result.Error,
              Level   => Level,
              Message => Message,
              Hint    => Hint);
   end Error_With_Data;

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

   ---------------------------
   -- Put_Image_Result_Data --
   ---------------------------

   procedure Put_Image_Result_Data
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Self   : Object_With_Data) is
   begin
      Output.Put ("([" & Self'Address'Image & " ] with");
      Alice.Env.Increase_Indent (Output);

      Output.New_Line;
      Output.Put ("Status  => " & Self.Status'Image);
      Output.New_Line;

      case Self.Status is
         when Alice.Result.Success =>
            Output.Put ("Value   => " & Self.Data'Image);
            Output.New_Line;

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
   end Put_Image_Result_Data;

end Alice.Result;
