-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Exceptions;
with GNAT.OS_Lib;

with Simple_Logging;

with Alice_Alire;
with Alice_Cmd;
with Alice_Cmd.Work.Source;
with Alice_User_Config;
with OS_Cmd_Alr;
with OS_Cmd_Curl;
with OS_Cmd_Git;

procedure Alice is
   package Log renames Simple_Logging;
begin
   Alice_Cmd.Execute;

exception
   when Command_Error : OS_Cmd_Alr.Command_Error | OS_Cmd_Curl.Command_Error
     | OS_Cmd_Git.Command_Error =>
      Log.Error (Ada.Exceptions.Exception_Message (Command_Error));
      GNAT.OS_Lib.OS_Exit (1);

   when Config_Error : Alice_User_Config.User_Config_Error =>
      Log.Error (Ada.Exceptions.Exception_Message (Config_Error));
      GNAT.OS_Lib.OS_Exit (2);

   when Alire_Error : Alice_Alire.Alire_Error =>
      Log.Error (Ada.Exceptions.Exception_Message (Alire_Error));
      GNAT.OS_Lib.OS_Exit (3);

   when Source_Error : Alice_Cmd.Work.Source.Command_Source_Error =>
      Log.Error (Ada.Exceptions.Exception_Message (Source_Error));
      GNAT.OS_Lib.OS_Exit (4);
end Alice;
