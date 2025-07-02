-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package is the top-level package for the Alice standard library.
--
--  The Alice standard library provides a set of default implementations of
--  Alice interfaces and other utility functions used in Alice application. It
--  is designed to be used as a starting point for building Alice
--  applications, providing a set of common functionalities that can be
--  extended or replaced as needed. For example, the Alice CLI binary uses the
--  Logger and the Error_Handler implementations provided by the Alice
--  standard library to log messages and handle errors during the execution of
--  use cases.
--
--  The Alice standard library is not intended to be used as a standalone
--  library, but rather as a set of utilities that can be used in Alice
--  applications to provide a consistent and reliable set of functionalities
--  according to the Clean Architecture principles.

with Alice.Context;
with Alice.OS_Context;

package Alice.Std is

   function Get_OS_Context return Alice.OS_Context.Object_Access;
   --  Get_OS_Context returns a standard OS context object that can be used to
   --  handle errors and log messages. It initializes the error handler and
   --  logger for the OS context.

   function Get_Context return Alice.Context.Object_Access;
   --  Get_Context returns a standard context object that can be used to
   --  handle errors, log messages, and execute OS commands. It initializes
   --  the error handler, logger, progress tracker, and OS commands for the
   --  context. The OS commands include the Alr, Git, and Curl commands.

end Alice.Std;
