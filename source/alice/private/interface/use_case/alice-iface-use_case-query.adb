-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package body Alice.IFace.Use_Case.Query is

   package body Result is

      use all type Alice.Result.Status_Type;

      -------------
      -- Success --
      -------------

      function Success (Answer : Alice.UString) return Object is
      begin
         return
            Result : constant Object (Alice.Result.Success) :=
              Object'
                (Alice.Controlled
                 with Status => Alice.Result.Success, Answer => Answer);
      end Success;

      -----------
      -- Error --
      -----------

      function Error
        (Level   : Alice.Result.Error_Level;
         Message : Alice.UString;
         Hint    : Alice.Hint.Id := Alice.Hint.None) return Object is
      begin
         return
            Result : constant Object (Alice.Result.Error) :=
              Object'
                (Alice.Controlled
                 with
                   Status  => Alice.Result.Error,
                   Level   => Level,
                   Message => Message,
                   Hint    => Hint);
      end Error;

      --  #REVIEW - Improve if needed
      --  procedure Put_Image_Use_Case_Query_Result
      --    (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      --     Self   : Object) is
      --  begin
      --     null;
      --  end Put_Image_Use_Case_Query_Result;

   end Result;

end Alice.IFace.Use_Case.Query;
