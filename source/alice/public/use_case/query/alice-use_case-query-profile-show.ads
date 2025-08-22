-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Alice.Use_Case.Query.Profile.Show is

   type Object is new Alice.Use_Case.Query.Profile.Object with null record;

   overriding
   function Run
     (Self : in out Object; Args : String := "")
      return Alice.Result.Object_With_Data'Class;

end Alice.Use_Case.Query.Profile.Show;
