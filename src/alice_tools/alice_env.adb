-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Alice_Cmd;

with OS_Cmd_Git; use OS_Cmd_Git;

with GNAT.AWK;
with GNAT.Regpat;

with Simple_Logging;

package body Alice_Env is

   package Log renames Simple_Logging;

   Alice_Root_Dir : Unbounded_String := To_Unbounded_String ("");

   -------------------------
   -- Is_Alice_Repository --
   -------------------------

   function Is_Alice_Repository (Report_Error : Boolean := True) return Boolean
   is
      Cmd_Git    : OS_Cmd_Git.Cmd_Type;
      Run_Output : OS_Cmd_Git.Run_Output_Type;
      Success    : Boolean;
   begin
      if not Cmd_Git.Init then
         return False;
      end if;

      Run_Output := Cmd_Git.Run ("remote -v");
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
      Run_Output.Clean;

      if Success then
         Log.Detail ("alice git repository detected");
      elsif Report_Error then
         Alice_Cmd.Abort_Execution
           ("'alice' must be invoked inside the alice git repository");
      end if;

      return Success;
   end Is_Alice_Repository;

   -----------------------
   -- Is_Alice_Root_Dir --
   -----------------------

   function Is_Alice_Root_Dir (Report_Error : Boolean := True) return Boolean
   is
      use Ada.Directories;
      Success : Boolean := False;
   begin
      if not Is_Alice_Repository (Report_Error => False) then
         return False;
      end if;

      Success :=
        Exists ("config") and then Kind ("config") = Directory
        and then Exists
          (Compose
             (Containing_Directory => "config", Name => "alice_config",
              Extension            => "ads"));

      if Success then
         Log.Detail ("found config/alice_config.ads");
         Alice_Root_Dir := To_Unbounded_String (Current_Directory);
         Log.Detail ("Alice_Root_Dir = " & Get_Alice_Root_Dir);
      elsif Report_Error then
         Alice_Cmd.Abort_Execution
           ("'alice' must be invoked from the Alice' root directory");
      end if;

      return Success;
   end Is_Alice_Root_Dir;

   ------------------------
   -- Get_Alice_Root_Dir --
   ------------------------

   function Get_Alice_Root_Dir return String is (To_String (Alice_Root_Dir));

end Alice_Env;
