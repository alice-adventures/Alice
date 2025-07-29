-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package defines the command use case for creating the member profile
--  from a GitHub token in the Alice application.

package Alice.Use_Case.Cmd.Profile.Token is

   type Object is new Alice.Use_Case.Cmd.Profile.Object with null record;

   overriding
   function Run
     (Self : in out Object; Token : String) return Alice.Result.Object'Class;

end Alice.Use_Case.Cmd.Profile.Token;
