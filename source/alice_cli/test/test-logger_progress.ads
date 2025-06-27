-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.Context;

package Test.Logger_Progress is

   procedure Activity_With_No_Messages
     (Ctx : Alice.Context.Object; Title : String; Length : Integer);

   procedure Activity_With_Messages
     (Ctx : Alice.Context.Object; Title : String; Length : Integer);

   procedure Bug_That_Throw_Exception (Ctx : Alice.Context.Object);

end Test.Logger_Progress;
