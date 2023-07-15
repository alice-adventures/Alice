-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

separate (Alice_Cmd.Setup.Config)
procedure Execute_Token (Token : String) is
   Old_User_Config, New_User_Config : Alice_User_Config.User_Config_Type;

   Config_File_Exists : Boolean :=
     Alice_User_Config.Has_User_Config_File (Report_Error => False);

   Success       : Boolean;
   Login_Changed : Boolean := False;
begin
   Log.Detail
     ("Retrieving info from GitHUb and git and saving new config file");

   if Config_File_Exists then
      if not Old_User_Config.Read_From_File (Report_Error => False) then
         Log.Warning ("Could not read current config file, ignoring");
         Config_File_Exists := False;
      end if;
   end if;

   if not New_User_Config.Get_Info_From_Token (Token) then
      return;
   end if;

   if Config_File_Exists then
      Log.Detail ("Keep SPDX_Id from old config file");
      Success :=
        New_User_Config.Set_SPDX (Old_User_Config.SPDX, Report_Error => False);
      if Success then
         Log.Detail
           ("keeping SPDX_Id '" & New_User_Config.SPDX &
            "' from old config file");
      else
         Log.Warning
           ("Could not save current SPDX Id '" & Old_User_Config.SPDX &
            "': invalid");
         Log.Warning ("Using default SPDX Id");
         Log.Warning ("Set a different one with 'alice config --license'");
         Log.Always ("");
      end if;
      Log.Debug ("New_User_Config.Set_SPDX = " & New_User_Config'Image);
   end if;

   if Config_File_Exists
     and then Old_User_Config.Login /= New_User_Config.Login
   then
      Login_Changed := True;
      pragma Style_Checks (off);
      Log.Error
        ("Token provided is associated to a different GitHub account:");
      Log.Error
        ("  • currently configured token is for user login '" &
         Old_User_Config.Login & "'");
      Log.Error
        ("  • provided token is for user login '" & New_User_Config.Login &
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
   New_User_Config.Show;

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
         User_Config_File : constant String :=
           Compose
             (Containing_Directory => Alice_User_Config.Config_Directory,
              Name                 => Alice_User_Config.User_Config_File);

         Backup_Config_File : constant String := User_Config_File & ".backup";
      begin
         Copy_File
           (User_Config_File, Backup_Config_File,
            Form => "preserve=all_attributes, mode=overwrite");
         Log.Detail ("User config file backup created");
      exception
         when Use_Error =>
            Log.Warning ("Could not make a backup copy of the config file");
      end;
   end if;

   if New_User_Config.Write_To_File then
      Log.Info ("New user configuration file saved");
   else
      Log.Error ("Could not save the new user configuration file");
   end if;
end Execute_Token;
