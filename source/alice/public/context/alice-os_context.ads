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
      --  The error handler for the OS commands. It is responsible for
      --  handling errors that occur during the execution of external ODS
      --  commands.

      Log : Alice.IFace.Logger.Object_Access;
      --  The logger for the OS commands. It is used to log messages related
      --  to the execution of external OS commands.
   end record;

   type Object_Access is not null access all Object'Class;

end Alice.OS_Context;
