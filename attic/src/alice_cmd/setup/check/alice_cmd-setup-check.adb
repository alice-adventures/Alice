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
with Ada.Text_IO;

with Alice_Participant;
with GitHub.Profile; use GitHub.Profile;
with OS_Cmd_Alr;
with OS_Cmd_Curl;
with OS_Cmd_Git;

package body Alice_Cmd.Setup.Check is

   package Dir renames Ada.Directories;
   package Participant renames Alice_Participant;

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

      --  Profile : Profile_Type;

      Has_Errors : Boolean := False;
   begin
      if Args_Length > 0 then
         Log.Warning ("Too many arguments, ignored");
      end if;

      if Alr_Cmd.Check then
         Log.Info ("Command alr found at " & Alr_Cmd.Path);
      else
         Has_Errors := True;
         Log.Error ("Command alr not found");
      end if;

      if Curl_Cmd.Check then
         Log.Info ("Command curl found at " & Curl_Cmd.Path);
      else
         Has_Errors := True;
         Log.Error ("Command curl not found");
      end if;

      if Git_Cmd.Check then
         Log.Info ("Command git found at " & Git_Cmd.Path);
      else
         Has_Errors := True;
         Log.Error ("Command git not found");
      end if;

      if not Participant.Has_Local_Profile then
         Log.Error ("Could not find the user config file");
         Has_Errors := True;
      end if;

      --  begin
      --     Participant.Load_Profile(Profile);
      --  exception
      --     when Participant.Participant_Error =>
      --        null;
      --  end;

      --  if not Profile.Read_From_File (Report_Error => False) then
      --     Log.Error ("Could not read the user config file");
      --     Has_Errors := True;
      --  end if;

      if Has_Errors then
         raise Command_Check_Error
           with "Solve the previous errors to continue";
      end if;

      Log.Info ("Checking GitHub access and git functionality");

      if Git.User_Has_Remote_Repository (Participant.Get_Local_Profile,
         "alice-test") then
         Log.Detail ("User has repo 'alice-test'");
      else
         Log.Detail ("Missing repo 'alice-test' for user, creating repo");
         if Git.Create_Remote_Repository
             (Participant.Get_Local_Profile, "alice-test",
              "Test repository used by Alice Adventures")
         then
            Log.Info ("Repository 'alice-test' created");
         else
            raise Command_Check_Error
              with "Could no create repository 'alice-test'";
         end if;
      end if;

      Dir.Set_Directory ("tmp");
      if Dir.Exists ("alice-test") then
         Log.Detail ("Local 'alice-test' repository removed");
         Dir.Delete_Tree ("alice-test");
      end if;

      if Alice_Git.Clone_Remote_Repository (Participant.Get_Local_Profile.Login & "/alice-test") then
         Log.Detail ("Repo alice-test cloned");
      else
         raise Command_Check_Error
           with "Could not clone '"
                & Participant.Get_Local_Profile.Login
                & "alice-test' repository";
      end if;

      Dir.Set_Directory ("alice-test");
      declare
         use Ada.Text_IO;
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

      if Git_Cmd.Run ("add README.md") = 0 then
         Log.Detail ("Changes staged for commit");
      else
         raise Command_Check_Error
           with "Could not work (add) with 'alice-test' repository";
      end if;

      if Git_Cmd.Run ("commit -q -m check") = 0 then
         Log.Detail ("Commit README.md");
      else
         raise Command_Check_Error
           with "Could not work (commit) with alice-test repository";
      end if;

      if Git_Cmd.Run ("push -q -u origin HEAD") = 0 then
         Log.Detail ("Pushed changes in README.md");
      else
         raise Command_Check_Error
           with "Could not work (push) with alice-test repository";
      end if;

      Log.Info ("Your environment is fully functional");
   end Execute;

end Alice_Cmd.Setup.Check;
