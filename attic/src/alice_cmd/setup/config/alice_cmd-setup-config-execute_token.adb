-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

separate (Alice_Cmd.Setup.Config)
procedure Execute_Token (Token : String) is
   Old_Profile, New_Profile : Profile_Type;

   Profile_Exists : constant Boolean := Participant.Has_Local_Profile;
   Login_Changed  : Boolean          := False;
begin
   Log.Detail
     ("Retrieving info from GitHUb and git and saving new config file");

   Participant.Load_Profile_From_Token (New_Profile, Token);

   if Profile_Exists then
      Log.Detail ("Keep SPDX_Id from old config file");
      Old_Profile := Participant.Get_Local_Profile;
      New_Profile.Set_SPDX (Old_Profile.SPDX);

      --  if Success then
      --     Log.Detail
      --   ("keeping SPDX_Id '" & New_Profile.SPDX & "' from old config file");
      --  else
      --     Log.Warning
      --       ("Could not save current SPDX Id '" & Old_Profile.SPDX &
      --        "': invalid");
      --     Log.Warning ("Using default SPDX Id");
      --     Log.Warning ("Set a different one with 'alice config --license'");
      --     Log.Always ("");
      --  end if;
      --  Log.Debug ("New_Profile.Set_SPDX = " & New_Profile'Image);
   end if;

   if Profile_Exists and then Old_Profile.Login /= New_Profile.Login then
      Login_Changed := True;
      pragma Style_Checks (off);
      Log.Error
        ("Token provided is associated to a different GitHub account:");
      Log.Error
        ("  * currently configured token is for user login '" &
         Old_Profile.Login & "'");
      Log.Error
        ("  * provided token is for user login '" & New_Profile.Login & "'");
      Log.Error
        ("Overwriting the configuration file with a different login can cause serious problems,");
      Log.Error
        ("both in your locally cloned repositories and in Alice participation.");
      Log.Error
        ("Answer 'Yes' ONLY if you are completely sure about what you are doing.");
      Log.Always ("");
      pragma Style_Checks (on);
   end if;

   Log.Info ("New configuration file contents is:");
   Participant.Show (New_Profile);

   declare
      Answer : CLIC.User_Input.Answer_Kind;
   begin
      Answer :=
        CLIC.User_Input.Query
          ("Do you want to continue?", Valid => [True, True, False],
           Default                           =>
             (if Login_Changed then CLIC.User_Input.No
              else CLIC.User_Input.Yes));
      if Answer = CLIC.User_Input.No then
         Log.Warning ("No changes applied");
         return;
      end if;
   end;

   if Profile_Exists then
      Log.Detail ("Make a backup copy of the user config file");
      declare
         use Ada.Directories;
         Profile_File : constant String :=
           Compose
             (Containing_Directory => Conf.Local.Config_Directory,
              Name                 => Conf.Local.Profile);

         Backup_Config_File : constant String := Profile_File & ".backup";
      begin
         Copy_File
           (Profile_File, Backup_Config_File,
            Form => "preserve=all_attributes, mode=overwrite");
         Log.Detail ("User config file backup created");
      exception
         when Use_Error =>
            Log.Warning ("Could not make a backup copy of the config file");
      end;
   end if;

   Participant.Save_Local_Profile (New_Profile);
end Execute_Token;
