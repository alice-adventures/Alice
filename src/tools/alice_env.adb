-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with OS_Cmd;
with OS_Cmd.Git; use OS_Cmd.Git;

with GNAT.AWK;
with GNAT.OS_Lib;
with GNAT.Regpat;

with Simple_Logging;

use all type GNAT.OS_Lib.String_Access;

package body Alice_Env is

   package Log renames Simple_Logging;

   -------------------------
   -- Is_Alice_Repository --
   -------------------------

   function Is_Alice_Repository (Report_Error : Boolean := True) return Boolean
   is
      OS_Cmd_Git : OS_Cmd_Git_Type;
      Run_Output : OS_Cmd.Run_Output_Type;
      Success    : Boolean;
   begin
      OS_Cmd_Git.Init;

      Run_Output := OS_Cmd_Git.Run ("remote -v");
      declare
         Alice_Repo_Matcher : constant GNAT.Regpat.Pattern_Matcher :=
           GNAT.Regpat.Compile ("^.*github\.com.alice-adventures.Alice\.git$");

         Matches : Natural := 0;

         procedure Origin_Match is
         begin
            Matches := @ + 1;
            Log.Debug ("AWK origin match #" & Matches'Image);
         end Origin_Match;

         procedure Repo_Match is
         begin
            Matches := @ + 1;
            Log.Debug
              ("AWK repo match #" & Matches'Image & ", " & GNAT.AWK.Field (2) &
               " " & GNAT.AWK.Field (3));
         end Repo_Match;
      begin
         GNAT.AWK.Add_File (Run_Output.Temp_File.all);
         GNAT.AWK.Register (1, "origin", Origin_Match'Unrestricted_Access);
         GNAT.AWK.Register
           (2, Alice_Repo_Matcher, Repo_Match'Unrestricted_Access);

         GNAT.AWK.Parse;
         GNAT.AWK.Close (GNAT.AWK.Default_Session.all);

         Success := (Matches = 4);
      end;
      OS_Cmd_Git.Clean (Run_Output);

      if Success then
         Log.Detail ("alice git repository detected");
      elsif Report_Error then
         Log.Error ("'alice' must be invoked inside the alice git repository");
      end if;

      return True;
   end Is_Alice_Repository;

   -----------------------
   -- Is_Alice_Root_Dir --
   -----------------------

   function Is_Alice_Root_Dir (Report_Error : Boolean := True) return Boolean
   is
      Success : Boolean := False;
   begin
      if not Is_Alice_Repository (Report_Error => False) then
         return False;
      end if;

      Success := GNAT.OS_Lib.Is_Directory ("config");
      if Success then
         declare
            Alice_Config_ADS : GNAT.OS_Lib.String_Access := null;
         begin
            Alice_Config_ADS :=
              GNAT.OS_Lib.Locate_Regular_File ("alice_config.ads", "config");
            Success          := (Alice_Config_ADS /= null);
            GNAT.OS_Lib.Free (Alice_Config_ADS);
         end;
      end if;

      if Success then
         Log.Detail ("found config/alice_config.ads");
      elsif Report_Error then
         Log.Error ("'alice' must be invoked from the Alice' root directory");
      end if;

      return Success;
   end Is_Alice_Root_Dir;

end Alice_Env;
