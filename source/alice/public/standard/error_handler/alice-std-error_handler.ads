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

   overriding
   function Initialize (Self : in out Object) return Alice.Result.Object'Class;

   overriding
   procedure Finalize (Self : in out Object);

   overriding
   function Handle_Error
     (Self : in out Object; Result : Alice.Result.Error_Object'Class)
      return Boolean;

   overriding
   procedure Exit_Application
     (Self    : in out Object;
      Result  : Alice.Result.Object'Class;
      Explain : Alice.UString := Alice.UStr (""))
   with No_Return;

   pragma No_Return (Exit_Application);

end Alice.Std.Error_Handler;
