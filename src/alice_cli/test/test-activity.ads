-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.Log.Activity;

package Test.Activity is

   procedure Success
     (Activity : in out Alice.Log.Activity.Object'Class;
      Title    : String;
      Length   : Integer);

   procedure Fatal_Error
     (Activity : in out Alice.Log.Activity.Object'Class);

end Test.Activity;
