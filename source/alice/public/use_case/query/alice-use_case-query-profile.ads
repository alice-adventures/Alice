-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.IFace.Use_Case.Query;
with Alice.Result;

package Alice.Use_Case.Query.Profile is

   type Object is abstract
     new Alice.Use_Case.Object
     and Alice.IFace.Use_Case.Query.Object
   with null record;

   overriding
   function Run
     (Self : in out Object; Args : String := "")
      return Alice.Result.Object_With_Data'Class is abstract;

end Alice.Use_Case.Query.Profile;
