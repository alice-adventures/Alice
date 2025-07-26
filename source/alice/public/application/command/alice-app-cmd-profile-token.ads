-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package defines the command use case for managing the user profile
--  token in the Alice application. It allows users to set up their profile
--  using a GitHub token, which is essential for accessing and managing their
--  GitHub profile information within the Alice application.

with Alice.App.Use_Case;
with Alice.Result;

package Alice.App.Cmd.Profile.Token is

   type Object is new Alice.App.Use_Case.Object with null record;

   overriding
   function Run
     (Self : in out Object; Token : String) return Alice.Result.Object'Class;

end Alice.App.Cmd.Profile.Token;
