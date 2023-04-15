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

package body Alice_Command.Setup is

   package Log renames Simple_Logging;

   -----------------
   -- Setup_Alice --
   -----------------

   procedure Setup_Alice is
      FD           : GNAT.OS_Lib.File_Descriptor;
      Status       : Integer;
      Str_Restore  : aliased String := "restore";
      Str_CWD      : aliased String := ".";
      Str_Checkout : aliased String := "checkout";
      Str_Main     : aliased String := "main";
   begin
      Log.Info ("Setup Alice: first time execution");
      FD := GNAT.OS_Lib.Create_File (".setup", GNAT.OS_Lib.Text);

      -- make sure any modified file is restored
      Status :=
        GNAT.OS_Lib.Spawn
          ("git", [Str_Restore'Unchecked_Access, Str_CWD'Unchecked_Access]);
      Log.Debug ("git restore returns" & Status'Image);

      -- switch to branch 'main'
      Status :=
        GNAT.OS_Lib.Spawn
          ("git", [Str_Checkout'Unchecked_Access, Str_Main'Unchecked_Access]);
      Log.Debug ("git checkout main returns" & Status'Image);
   end Setup_Alice;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Cmd : in out Cmd_Type; Args : AAA.Strings.Vector)
   is
      Exists_Setup_File : Boolean := GNAT.OS_Lib.Is_Readable_File (".setup");
   begin
      if not Exists_Setup_File then
         Setup_Alice;
      end if;

   end Execute;

end Alice_Command.Setup;
