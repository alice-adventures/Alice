-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GNAT.OS_Lib;
with GNAT.Directory_Operations;
with Simple_Logging;

use all type GNAT.OS_Lib.String_Access;

package body Alice_Setup is

   package Log renames Simple_Logging;

   ----------------
   -- Initialize --
   ----------------

   function Initialize return Boolean is
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
         Log.Error ("'git' cannot be found in PATH");
         return False;
      else
         Log.Debug ("found 'git' at '" & Git_Cmd.all & "'");
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
   end Initialize;

   ------------------
   -- Check_Status --
   ------------------

   function Check_Status return Boolean is
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
   end Check_Status;

end Alice_Setup;
