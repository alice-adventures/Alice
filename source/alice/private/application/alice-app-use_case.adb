-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package body Alice.App.Use_Case is

   -------------
   -- Context --
   -------------

   overriding
   function Context (Self : in out Object) return Alice.Context.Object_Access
   is (Self.Context);

   -------------
   -- Context --
   -------------

   overriding
   procedure Context
     (Self : in out Object; Context : Alice.Context.Object_Access) is
   begin
      Self.Context := Context;
   end Context;

   ---------
   -- Run --
   ---------

   overriding
   function Run
     (Self : in out Object; Args : String := "")
      return Alice.Result.Object'Class is
   begin
      return Result : Alice.Result.Success_Object;
   end Run;

end Alice.App.Use_Case;
