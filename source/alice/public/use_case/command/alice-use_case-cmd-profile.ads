-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package defines the command use case for managing the user profile in
--  the Alice application.

with Alice.Result;

package Alice.Use_Case.Cmd.Profile is

   type Object is abstract new Alice.Use_Case.Cmd.Object with null record;

   overriding
   function Run
     (Self : in out Object; Args : String := "")
      return Alice.Result.Object'Class is abstract;

end Alice.Use_Case.Cmd.Profile;
