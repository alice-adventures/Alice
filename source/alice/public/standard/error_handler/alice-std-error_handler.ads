-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package implements the standard error handler for the Alice
--  application.

with GNAT.Source_Info;

with Alice.IFace.Error_Handler;
with Alice.Result;

package Alice.Std.Error_Handler is

   type Object is new Alice.IFace.Error_Handler.Object with null record;

   overriding
   procedure Log
     (Self     : in out Object;
      Message  : String;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity;
      Location : String := GNAT.Source_Info.Source_Location);

   overriding
   function Handle_Error
     (Self : in out Object; Result : Alice.Result.Error_Object'Class)
      return Boolean;

   overriding
   procedure Exit_Application
     (Self    : in out Object;
      Level   : Alice.Result.Error_Level;
      Explain : Alice.UString := Alice.Null_UString)
   with No_Return;

   overriding
   procedure Exit_Application
     (Self    : in out Object;
      Result  : Alice.Result.Object'Class;
      Explain : Alice.UString := Alice.Null_UString)
   with No_Return;

end Alice.Std.Error_Handler;
