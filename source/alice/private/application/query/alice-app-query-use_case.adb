-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package defines the top-level package for the queries in the Alice
--  application.

package body Alice.App.Query.Use_Case is

   ----------------
   -- Initialize --
   ----------------

   overriding
   function Initialize (Self : in out Object) return Alice.Result.Object'Class
   is
   begin
      return Result : Alice.Result.Success_Object;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Self : in out Object) is null;

   ------------
   -- Answer --
   ------------

   function Answer (Self : Object) return Alice.UString
   is (Self.Answer);

   ------------
   -- Answer --
   ------------

   procedure Answer (Self : in out Object; Value : Alice.UString) is
   begin
      Self.Answer := Value;
   end Answer;

   ---------
   -- Run --
   ---------

   overriding
   function Run
     (Self : in out Object; Args : String := "")
      return Alice.Result.Object'Class is
   begin
      Self.Answer := Alice.UStr ("Default, empty implementation");
      return Result : Alice.Result.Success_Object;
   end Run;

end Alice.App.Query.Use_Case;
