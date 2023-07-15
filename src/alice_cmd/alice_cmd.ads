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

end Alice_Cmd;
