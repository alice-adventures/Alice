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

with Alice.IFace;
with Alice.IFace.Logger.Progress;
with Alice.OS_Context;

package Alice.Context is

   type OS_Commands is record
      null;
      --  Alr  : Alice.IFace.OS_Cmd.Object_Access;
      --  Git  : Alice.IFace.OS_Cmd.Object_Access;
      --  Curl : Alice.IFace.OS_Cmd.Object_Access;
   end record;

   type Object is new OS_Context.Object with record
      Progress : Alice.IFace.Logger.Progress.Object_Access;
      --  The progress logger for the application context. It is used to log
      --  progress messages related to long-running operations or tasks.

      --  OS_Cmd : OS_Commands;
      --  The OS commands for the application context. It contains references
      --  to the command objects for various OS commands used in the
      --  application, such as Alr, Git, and Curl.
   end record;

   procedure Init (Self : in out Object'Class);

end Alice.Context;
