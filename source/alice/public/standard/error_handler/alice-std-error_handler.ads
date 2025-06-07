-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package implements the standard error handler for the Alice
--  application.

with Alice.IFace.Error_Handler;
with Alice.Result;

package Alice.Std.Error_Handler is

   type Object is new Alice.IFace.Error_Handler.Object with null record;
   --  The class type for the error handler. It is used to define the type of
   --  the error handler object.

   overriding
   function Handle_Error
     (Self : Object; Result : Alice.Result.Object_Access) return Boolean;

   overriding
   procedure Exit_Application
     (Self : Object; Result : Alice.Result.Object_Access);

   pragma No_Return (Exit_Application);

end Alice.Std.Error_Handler;
