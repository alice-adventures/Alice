-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package defines the command use case for updating the user profile
--  token in the Alice application. It allows users to refresh their profile
--  in case of changes or updates, ensuring that the profile information is
--  always up-to-date and reflects the latest state of the user's profile.

package Alice.Use_Case.Cmd.Profile.Update is

   type Object is new Alice.Use_Case.Cmd.Object with null record;

   overriding
   function Run
     (Self : in out Object; Args : String) return Alice.Result.Object'Class;

end Alice.Use_Case.Cmd.Profile.Update;
