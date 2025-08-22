-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package implements the version query use case.

with Alice.Result;

package Alice.Use_Case.Query.Version is

   type Object is new Alice.Use_Case.Query.Object with null record;

   overriding
   function Run
     (Self : in out Object; Args : String := "")
      return Alice.Result.Object_With_Data'Class;
   --  This function retrieves the version of the Alice application. It
   --  returns a result with the version information.

end Alice.Use_Case.Query.Version;
