-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

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

end Alice.Result;
