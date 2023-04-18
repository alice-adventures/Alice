-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with Simple_Logging;

use all type GNAT.OS_Lib.String_Access;

package body Alice_Command.Setup is

   package Log renames Simple_Logging;

   -----------------
   -- Setup_Alice --
   -----------------

   function Setup_Alice return Boolean is
      Git_Cmd : GNAT.OS_Lib.String_Access;

      Str_Restore  : aliased String := "restore";
      Str_CWD      : aliased String := ".";
      Str_Checkout : aliased String := "checkout";
      Str_Main     : aliased String := "main";

      Status : Integer;
   begin
      Log.Info ("first time run: configuring Alice");

      Git_Cmd := GNAT.OS_Lib.Locate_Exec_On_Path ("git");
      if Git_Cmd = null then
         Log.Error ("git cannot be found in PATH");
         return False;
      else
         Log.Debug ("found git at '" & Git_Cmd.all & "'");
      end if;

      --  create .setup directory
      Log.Detail ("create .setup directory");
      begin
         GNAT.Directory_Operations.Make_Dir (".setup");
      exception
         when GNAT.Directory_Operations.Directory_Error =>
            Log.Error ("could not create directory '.setup'");
            return False;
      end;

      --  make sure any modified file is restored
      Log.Detail ("restoring files modified by alr");
      Status :=
        GNAT.OS_Lib.Spawn
          (Git_Cmd.all,
           [Str_Restore'Unchecked_Access, Str_CWD'Unchecked_Access]);
      Log.Debug ("git restore returns" & Status'Image);

      --  switch to branch 'main'
      Log.Detail ("checkout branch main");
      Status :=
        GNAT.OS_Lib.Spawn
          (Git_Cmd.all,
           [Str_Checkout'Unchecked_Access, Str_Main'Unchecked_Access]);
      Log.Debug ("git checkout main returns" & Status'Image);

      return True;
   end Setup_Alice;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Cmd : in out Cmd_Type; Args : AAA.Strings.Vector)
   is
      Exists_Setup_Directory : constant Boolean :=
        GNAT.OS_Lib.Is_Directory (".setup");
   begin
      if not Exists_Setup_Directory then
         Log.Detail ("setup directory not found");
         if not Setup_Alice then
            Log.Always ("Alice could not be configured, aborting");
            return;
         end if;
      else
         Log.Detail ("alice already setup");
      end if;

   end Execute;

end Alice_Command.Setup;
