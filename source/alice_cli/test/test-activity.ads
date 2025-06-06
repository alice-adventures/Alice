-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.Context;

package Test.Activity is

   procedure Success
     (Ctx : Alice.Context.Object; Title : String; Length : Integer);

   procedure Fatal_Error (Ctx : Alice.Context.Object);

end Test.Activity;
