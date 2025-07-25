-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.App.Use_Case;
with Alice.Result;

package Alice.App.Cmd.Profile.Token is

   type Object is new Alice.App.Use_Case.Object with null record;

   overriding
   function Run
     (Self : in out Object; Token : String) return Alice.Result.Object'Class;

end Alice.App.Cmd.Profile.Token;
