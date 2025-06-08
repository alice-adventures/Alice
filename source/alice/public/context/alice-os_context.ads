-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package defines the Context type used to represent the application
--  context in the Alice application. It includes a logger for logging
--  messages and an error handler for handling errors that occur during the
--  execution of use cases. The context is used to pass information between
--  use cases and to manage the application's state.

with Alice.IFace.Error_Handler;
with Alice.IFace.Logger;

package Alice.OS_Context is

   type Object is tagged record
      Err : Alice.IFace.Error_Handler.Object_Access;
      --  The error handler for the application context. It is responsible for
      --  handling errors that occur during the execution of use cases.

      Log : Alice.IFace.Logger.Object_Access;
      --  The logger for the application context. It is used to log messages
      --  related to the execution of use cases and other application events.
   end record;

   procedure Init (Self : in out Object'Class) is abstract;
   --  Initialize the OS context. This procedure should be called before using
   --  any OS commands or logging functionality. It sets up the error handler
   --  and logger for the context.

end Alice.OS_Context;
