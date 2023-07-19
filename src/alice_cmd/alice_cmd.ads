-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Alice_Cmd is

   procedure Abort_Execution (Error : String; Exit_Status : Integer := 1);
   --  Logs the given error and stops execution of the Alice command.
   --  Immediately returns control the to calling parent (script or command
   --  line).

   procedure Execute;
   --  Execute the Alice command specified by the arguments.

   procedure Check_Unique_Subcommand (Number : Positive);
   --  Check that only one subcommand has been specified.

   procedure Check_Argument_Length (Number, Length : Positive);
   --  Check that the Number of arguments is exactly the Length required.

   procedure Subcommand_Not_Found;
   --  Emits an error when a subcommand cannot be found.

end Alice_Cmd;
