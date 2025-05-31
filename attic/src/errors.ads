-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice_Alire;
with Alice_Cmd;
with Alice_Cmd.Setup.Check;
with Alice_Cmd.Setup.Config;
with Alice_Cmd.Work.Source;
--  with Alice_Participant;
with OS_Cmd_Alr;
with OS_Cmd_Curl;
with OS_Cmd_Git;

package Errors is

   package Setup_Check renames Alice_Cmd.Setup.Check;
   package Setup_Config renames Alice_Cmd.Setup.Config;
   package Work_Source renames Alice_Cmd.Work.Source;

   Alire            : exception renames Alice_Alire.Alire_Error;
   Command_Alice    : exception renames Alice_Cmd.Command_Alice_Error;
   Command_Check    : exception renames Setup_Check.Command_Check_Error;
   Command_Config   : exception renames Setup_Config.Command_Config_Error;
   Command_Source   : exception renames Work_Source.Command_Source_Error;
   OS_Command_Alire : exception renames OS_Cmd_Alr.Command_Error;
   OS_Command_Curl  : exception renames OS_Cmd_Curl.Command_Error;
   OS_Command_Git   : exception renames OS_Cmd_Git.Command_Error;
   --  User_Config  : exception renames Alice_Participant.User_Config_Error;

end Errors;
