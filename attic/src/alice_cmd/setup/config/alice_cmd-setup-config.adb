-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;

with Simple_Logging;
with CLIC.User_Input;

with Alice_Participant;
with GitHub.Profile; use GitHub.Profile;

package body Alice_Cmd.Setup.Config is

   package Log renames Simple_Logging;
   package Participant renames Alice_Participant;

   use all type CLIC.User_Input.Answer_Kind;

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Cmd_Type;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
   begin
      --!pp off
      CLIC.Subcommand.Define_Switch
        (Config      => Config,
         Output      => Cmd.Show'Access,
         Switch      => "-s",
         Long_Switch => "--show",
         Help        => "Show user configuration");

      CLIC.Subcommand.Define_Switch
        (Config      => Config,
         Output      => Cmd.Refresh'Access,
         Switch      => "-r",
         Long_Switch => "--refresh",
         Help        => "Refresh user configuration");

      CLIC.Subcommand.Define_Switch
        (Config      => Config,
         Output      => Cmd.Token'Access,
         Switch      => "-t",
         Long_Switch => "--token",
         Help        => "Set user config from GitHub token");

      CLIC.Subcommand.Define_Switch
        (Config      => Config,
         Output      => Cmd.License'Access,
         Switch      => "-l",
         Long_Switch => "--license",
         Help        => "Set SPDX license ID for your work");
      --!pp on
   end Setup_Switches;

   ------------------
   -- Execute_Show --
   ------------------

   procedure Execute_Show is
      Profile : Profile_Type;
   begin
      if not Participant.Has_Local_Profile then
         Log.Info ("User profile not found");
         return;
      end if;

      Participant.Load_Local_Profile_Into (Profile);

      Log.Info ("User profile is:");
      Participant.Show (Profile);
   end Execute_Show;

   ---------------------
   -- Execute_Refresh --
   ---------------------

   procedure Execute_Refresh is
      New_Profile : Profile_Type;
      Old_Profile : Profile_Type;
   begin
      if not Participant.Has_Local_Profile then
         raise Participant.Participant_Error
           with "Profile not found, cannot refresh";
      end if;

      Participant.Load_Local_Profile_Into (Old_Profile);
      Participant.Load_Profile_From_Token (New_Profile, Old_Profile.Token);
      Log.Debug ("New_Profile from Token =" & New_Profile'Image);

      if Old_Profile.Login /= New_Profile.Login then
         raise Participant.Participant_Error
           with "Cannot change GitHub login, " &
           "run 'alice config --token' instead";
      end if;

      New_Profile.Set_SPDX (Old_Profile.SPDX);
      --  if not Success then
      --     Log.Warning
      --       ("Could not keep invalid SPDX Id '" & Old_Profile.SPDX &
      --        "', default 'MIT' applied");
      --  end if;

      if New_Profile /= Old_Profile then
         Participant.Save_Local_Profile (New_Profile);
         Log.Info ("New profile saved");
         Participant.Show (New_Profile);
      else
         Log.Info ("No changes detected, profile unchanged");
      end if;

   exception
      when GitHub.Profile.GitHub_Profile_Error =>
         raise Participant.Participant_Error
           with "Run 'alice config --token' with a valid token";
   end Execute_Refresh;

   -------------------
   -- Execute_Token --
   -------------------

   procedure Execute_Token (Token : String) is separate;

   ---------------------
   -- Execute_License --
   ---------------------

   procedure Execute_License (SPDX_Id : String) is
      Profile : Profile_Type;
   begin
      if not Participant.Has_Local_Profile then
         raise Participant.Participant_Error
           with "Profile not found, cannot set SPDX";
      end if;

      Profile := Participant.Get_Local_Profile;
      Log.Debug ("Profile =" & Profile'Image);

      if Profile.SPDX = SPDX_Id then
         Log.Warning ("Same SPDX Id specified, nothing changed");
      else
         Profile.Set_SPDX (SPDX_Id);
         Log.Debug ("Profile.Set_SPDX =" & Profile'Image);
         Participant.Save_Local_Profile (Profile);

         --  if Success then
         --     Success := Profile.Write_To_File;
         --     Log.Info
         --       ("New SPDX license Id '" & SPDX_Id &
         --        "' will be applied from now on");
         --  else
         --     Log.Error ("Choose a valid Id from https://spdx.org/licenses");
         --  end if;
      end if;
   end Execute_License;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Cmd : in out Cmd_Type; Args : AAA.Strings.Vector)
   is
      Actions     : Natural          := 0;
      Args_Length : constant Natural := Natural (Args.Length);
   begin
      Actions := @ + (if Cmd.Show then 1 else 0);
      Actions := @ + (if Cmd.Refresh then 1 else 0);
      Actions := @ + (if Cmd.Token then 1 else 0);
      Actions := @ + (if Cmd.License then 1 else 0);

      if Actions = 0 then
         --  default command is '--show'
         Cmd.Show := True;
      else
         Check_Unique_Subcommand (Actions);
      end if;

      if Actions > 1 then
         raise Command_Config_Error with "Specify only one subcommand";
      end if;

      if Cmd.Show then
         Execute_Show;
      elsif Cmd.Refresh then
         Execute_Refresh;
      elsif Cmd.Token or else Cmd.License then
         Check_Argument_Length (Args_Length, 1);
         if Cmd.Token then
            Execute_Token (Args.First_Element);
         else
            Execute_License (Args.First_Element);
         end if;
      else
         Subcommand_Not_Found;
      end if;
   end Execute;

end Alice_Cmd.Setup.Config;
