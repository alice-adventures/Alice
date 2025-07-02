-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package defines the top-level package for the implementation of Alice
--  use case.

with Alice.Context;
with Alice.IFace.Use_Case;
with Alice.Result;

package Alice.App.Use_Case is

   type Object is new Alice.IFace.Use_Case.Object with private;

   overriding
   function Initialize (Self : in out Object) return Alice.Result.Object'Class;

   overriding
   procedure Finalize (Self : in out Object);

   overriding
   function Context (Self : in out Object) return Alice.Context.Object_Access;

   overriding
   procedure Context (Self : in out Object; Ctx : Alice.Context.Object_Access);

   overriding
   function Run
     (Self : in out Object; Args : String := "")
      return Alice.Result.Object'Class;

private

   type Object is new Alice.IFace.Use_Case.Object with record
      Context : Alice.Context.Object_Access;
   end record;

end Alice.App.Use_Case;
