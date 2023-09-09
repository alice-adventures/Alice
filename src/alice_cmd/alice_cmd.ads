-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Simple_Logging;

with Alice_Configuration;
with Alice_Environment;
with Alice_Git;
with Alice_User_Config;

package Alice_Cmd is

   package Conf renames Alice_Configuration;
   package Env renames Alice_Environment;
   package Git renames Alice_Git;
   package Log renames Simple_Logging;
   package Usr renames Alice_User_Config;

   procedure Abort_Execution (Error : String; Exit_Status : Integer := 1);
   --  Logs the given error and stops execution of the Alice command.
   --  Immediately returns control the to calling parent (script or command
   --  line).

   procedure Execute;
   --  Execute the Alice command specified by the arguments.

   procedure Check_Unique_Subcommand (Number : Natural);
   --  Check that only one subcommand has been specified.

   procedure Check_Argument_Length (Number, Length : Natural);
   --  Check that the Number of arguments is exactly the Length required.

   procedure Subcommand_Not_Found;
   --  Emits an error when a subcommand cannot be found.

end Alice_Cmd;
