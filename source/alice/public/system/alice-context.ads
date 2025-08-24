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

with Ada.Strings.Text_Buffers;

with Alice.IFace;
with Alice.IFace.Error_Handler;
with Alice.IFace.Logger;
with Alice.IFace.Progress_Tracker;
with Alice.OS_Commands;

package Alice.Context is

   type Object is tagged record
      Err : Alice.IFace.Error_Handler.Object_Access;
      --  The error handler for the application context. It is responsible for
      --  handling errors that occur during the execution of use cases.

      Log : Alice.IFace.Logger.Object_Access;
      --  The logger for the application context. It is used to log messages
      --  related to the execution of use cases and other application events.

      Progress : Alice.IFace.Progress_Tracker.Object_Access;
      --  The progress logger for the application context. It is used to log
      --  progress messages related to long-running operations or tasks.

      OS_Cmd : Alice.OS_Commands.Object;
      --  The OS commands for the application context. It contains references
      --  to the command objects for various OS commands used in the
      --  application, such as Alr, Git, and Curl.
   end record
   with Put_Image => Put_Image_Context;

   procedure Put_Image_Context
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Self   : Object);

   type Object_Access is not null access all Alice.Context.Object'Class;

end Alice.Context;
