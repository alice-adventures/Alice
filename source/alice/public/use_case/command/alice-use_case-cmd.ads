-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package is the top-level package for the command use cases in the
--  Alice application. It defines the abstract type and the Run function that
--  all command use cases must implement.

with Alice.IFace.Use_Case.Cmd;
with Alice.Result;

package Alice.Use_Case.Cmd is

   type Object is abstract
     new Alice.Use_Case.Object
     and Alice.IFace.Use_Case.Cmd.Object
   with null record;

   overriding
   function Run
     (Self : in out Object; Args : String := "")
      return Alice.Result.Object'Class is abstract;

end Alice.Use_Case.Cmd;
