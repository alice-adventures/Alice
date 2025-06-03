-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.IFace.Error_Handler;
with Alice.IFace.Logger;

package Alice.Context is

   type Object is tagged record
      --  Error_Handler : Alice.IFace.Error_Handler.Object_Access;
      --  The error handler for the application context. It is responsible for
      --  handling errors that occur during the execution of use cases.

      Log : Alice.IFace.Logger.Object_Access;
      --  The logger for the application context. It is used to log messages
      --  related to the execution of use cases and other application events.
   end record;

   type Object_Access is not null access all Object'Class;
   --  The class type for the application context. It is used to define the
   --  type of the application context object.

end Alice.Context;
