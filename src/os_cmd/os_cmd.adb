-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Simple_Logging;

package body OS_Cmd is

   package Log renames Simple_Logging;

   ----------
   -- Init --
   ----------

   procedure Init (Cmd : in out OS_Cmd_Type; Cmd_Name : String) is
   begin
      Cmd.OS_Path := GNAT.OS_Lib.Locate_Exec_On_Path (Cmd_Name);
      if Cmd.OS_Path = null then
         Log.Error ("'" & Cmd_Name & "' cannot be found in PATH");
         GNAT.OS_Lib.OS_Exit (1);
      else
         Log.Debug ("found '" & Cmd_Name & "'' at '" & Cmd.OS_Path.all & "'");
      end if;
   end Init;

end OS_Cmd;
