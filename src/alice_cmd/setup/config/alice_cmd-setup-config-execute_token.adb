-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

separate (Alice_Cmd.Setup.Config)
procedure Execute_Token (Token : String) is
   Old_Profile, New_Profile : Alice_Participant.Profile_Type;

   Config_File_Exists : Boolean :=
     Alice_Participant.Has_Profile_File (Report_Error => False);

   Success       : Boolean;
   Login_Changed : Boolean := False;
begin
   Log.Detail
     ("Retrieving info from GitHUb and git and saving new config file");

   if Config_File_Exists then
      if not Old_Profile.Read_From_File (Report_Error => False) then
         Log.Warning ("Could not read current config file, ignoring");
         Config_File_Exists := False;
      end if;
   end if;

   if not New_Profile.Get_Info_From_Token (Token) then
      return;
   end if;

   if Config_File_Exists then
      Log.Detail ("Keep SPDX_Id from old config file");
      Success :=
        New_Profile.Set_SPDX (Old_Profile.SPDX, Report_Error => False);
      if Success then
         Log.Detail
           ("keeping SPDX_Id '" & New_Profile.SPDX &
            "' from old config file");
      else
         Log.Warning
           ("Could not save current SPDX Id '" & Old_Profile.SPDX &
            "': invalid");
         Log.Warning ("Using default SPDX Id");
         Log.Warning ("Set a different one with 'alice config --license'");
         Log.Always ("");
      end if;
      Log.Debug ("New_Profile.Set_SPDX = " & New_Profile'Image);
   end if;

   if Config_File_Exists
     and then Old_Profile.Login /= New_Profile.Login
   then
      Login_Changed := True;
      pragma Style_Checks (off);
      Log.Error
        ("Token provided is associated to a different GitHub account:");
      Log.Error
        ("  • currently configured token is for user login '" &
         Old_Profile.Login & "'");
      Log.Error
        ("  • provided token is for user login '" & New_Profile.Login &
         "'");
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
   New_Profile.Show;

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

   if Config_File_Exists then
      Log.Detail ("Make a backup copy of the user config file");
      declare
         use Ada.Directories;
         Profile_File : constant String :=
           Compose
             (Containing_Directory => Conf.Local.Config_Directory,
              Name                 => Conf.Local.Config_File);

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

   if New_Profile.Write_To_File then
      Log.Info ("New user configuration file saved");
   else
      Log.Error ("Could not save the new user configuration file");
   end if;
end Execute_Token;
