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

with Alice_Cmd;
with Errors; use Errors;

procedure Alice is
   package Log renames Simple_Logging;
begin

   Alice_Cmd.Execute;

exception
   when Error : Alire =>
      Log.Error (Ada.Exceptions.Exception_Message (Error));
      GNAT.OS_Lib.OS_Exit (1);

   when Error : Command_Alice =>
      Log.Error (Ada.Exceptions.Exception_Message (Error));
      GNAT.OS_Lib.OS_Exit (2);

   when Error : Command_Check =>
      Log.Error (Ada.Exceptions.Exception_Message (Error));
      GNAT.OS_Lib.OS_Exit (3);

   when Error : Command_Config =>
      Log.Error (Ada.Exceptions.Exception_Message (Error));
      GNAT.OS_Lib.OS_Exit (4);

   when Error : Command_Source =>
      Log.Error (Ada.Exceptions.Exception_Message (Error));
      GNAT.OS_Lib.OS_Exit (5);

   when Error : OS_Command_Alire | OS_Command_Curl | OS_Command_Git =>
      Log.Error (Ada.Exceptions.Exception_Message (Error));
      GNAT.OS_Lib.OS_Exit (6);

   --  when Error : User_Config =>
   --     Log.Error (Ada.Exceptions.Exception_Message (Error));
   --     GNAT.OS_Lib.OS_Exit (7);
end Alice;
