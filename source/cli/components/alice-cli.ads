-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package contains the main components of the Alice CLI application.

package Alice.CLI is

   procedure Initialize;
   --  Initialize the Alice CLI application. This registers the CLI
   --  subcommands.

   procedure Execute;
   --  Execute the Alice CLI application. This is the main entry point of the
   --  application. It initializes the context, sets the global switches,
   --  and executes the CLI command.

end Alice.CLI;
