-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Calendar.Formatting;
with Ada.Directories;

with Alice_User_Config; use Alice_User_Config;
with Alice_Git;         use Alice_Git;

with OS_Cmd_Alr;
with OS_Cmd_Curl;
with OS_Cmd_Git;

with Simple_Logging;

with Text_IO;

package body Alice_Cmd.Setup.Check is

   package Dir renames Ada.Directories;
   package Log renames Simple_Logging;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Cmd : in out Cmd_Type; Args : AAA.Strings.Vector)
   is
      Args_Length : constant Natural := Natural (Args.Length);

      Alr_Cmd  : OS_Cmd_Alr.Cmd_Type;
      Curl_Cmd : OS_Cmd_Curl.Cmd_Type;
      Git_Cmd  : OS_Cmd_Git.Cmd_Type;

      User_Config : Alice_User_Config.User_Config_Type;
      Run_Output  : OS_Cmd_Git.Run_Output_Type;
   begin
      if Args_Length > 0 then
         Log.Warning ("Too many arguments, ignored");
      end if;

      if Alr_Cmd.Init then
         Log.Info ("alr  command found at '" & Alr_Cmd.Path & "'");
      end if;

      if Curl_Cmd.Init then
         Log.Info ("curl command found at '" & Curl_Cmd.Path & "'");
      end if;

      if Git_Cmd.Init then
         Log.Info ("git  command found at '" & Git_Cmd.Path & "'");
      end if;

      if not Alice_User_Config.Has_User_Config_File then
         Alice_Cmd.Exit_Status := 1;
         return;
      end if;

      if not User_Config.Read_From_File then
         Alice_Cmd.Exit_Status := 1;
         return;
      end if;

      Log.Info ("Checking GitHub access and git functionality");

      if User_Has_GitHub_Repository (User_Config, "alice-test") then
         Log.Detail ("User has repo 'alice-test'");
      else
         Log.Detail ("Missing repo 'alice-test' for user, creating repo");
         if Create_GitHub_Repository
             (User_Config, "alice-test",
              "Test repository used by Alice Adventures")
         then
            Log.Info ("Repository 'alice-test' created");
         else
            Alice_Cmd.Exit_Status := 1;
            Log.Error ("Could no create repository 'alice-test'");
            return;
         end if;
      end if;

      Dir.Set_Directory ("tmp");
      if Dir.Exists ("alice-test") then
         Log.Detail ("Local 'alice-test' repository removed");
         Dir.Delete_Tree ("alice-test");
      end if;

      if Alice_Git.Clone_GitHub_Repository
          (Alice_Git.GitHub_Root & User_Config.Login & "/alice-test")
      then
         Log.Detail ("Repo alice-test cloned");
      else
         Alice_Cmd.Exit_Status := 1;
         Log.Error
           ("Could not clone '" & User_Config.Login &
            "alice-test' repository");
         return;
      end if;

      Dir.Set_Directory ("alice-test");
      declare
         use Text_IO;
         Readme   : File_Type;
         Time_Str : constant String :=
           Ada.Calendar.Formatting.Image (Ada.Calendar.Clock);
      begin
         Readme.Open (Append_File, "README.md");
         Readme.New_Line;
         Readme.Put_Line
           ("> check : `" & Time_Str (1 .. 10) & "T" & Time_Str (12 .. 19) &
            "Z`");
         Readme.Close;
         Log.Detail ("New check line added to README.md");
      end;

      Run_Output := Git_Cmd.Run ("add README.md");
      Run_Output.Clean;
      if Run_Output.Return_Code = 0 then
         Log.Detail ("Changes staged for commit");
      else
         Alice_Cmd.Exit_Status := 1;
         Log.Debug ("Could not work with this git repository");
         return;
      end if;

      Run_Output := Git_Cmd.Run ("commit -m check");
      Run_Output.Clean;
      if Run_Output.Return_Code = 0 then
         Log.Detail ("Commit README.md");
      else
         Alice_Cmd.Exit_Status := 1;
         Log.Debug ("Could not work with this git repository");
         return;
      end if;

      Run_Output := Git_Cmd.Run ("push -u origin HEAD");
      Run_Output.Clean;
      if Run_Output.Return_Code = 0 then
         Log.Detail ("Pushed changes in README.md");
      else
         Alice_Cmd.Exit_Status := 1;
         Log.Debug ("Could not work with this git repository");
         return;
      end if;

      Log.Info ("Your environment is fully functional");
   end Execute;

end Alice_Cmd.Setup.Check;
