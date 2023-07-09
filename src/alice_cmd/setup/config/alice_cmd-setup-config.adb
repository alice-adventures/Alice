-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Alice_User_Config;

with CLIC.User_Input;
use all type CLIC.User_Input.Answer_Kind;

with GNAT.Directory_Operations;
with GNAT.OS_Lib;
use all type GNAT.OS_Lib.String_Access;

with Simple_Logging;

package body Alice_Cmd.Setup.Config is

   package Log renames Simple_Logging;

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
         Long_Switch => "--show",
         Help        => "Show user configuration");

      CLIC.Subcommand.Define_Switch
        (Config      => Config,
         Output      => Cmd.Refresh'Access,
         Long_Switch => "--refresh",
         Help        => "Refresh user configuration from");

      CLIC.Subcommand.Define_Switch
        (Config      => Config,
         Output      => Cmd.Token'Access,
         Long_Switch => "--token",
         Help        => "Set user config from GitHub token");

      CLIC.Subcommand.Define_Switch
        (Config      => Config,
         Output      => Cmd.License'Access,
         Long_Switch => "--license",
         Help        => "Set SPDX license ID for your work");
      --!pp on
   end Setup_Switches;

   ------------------
   -- Execute_Show --
   ------------------

   procedure Execute_Show is
      User_Config : Alice_User_Config.User_Config_Type;
   begin
      if not Alice_User_Config.Has_User_Config_File then
         return;
      end if;

      User_Config := Alice_User_Config.Read_From_File;

      Log.Always ("User configuration file contents is:");
      User_Config.Show;
   end Execute_Show;

   ---------------------
   -- Execute_Refresh --
   ---------------------

   procedure Execute_Refresh is
   begin
      null;
   end Execute_Refresh;

   -------------------
   -- Execute_Token --
   -------------------

   procedure Execute_Token (Token : Unbounded_String) is
      Old_User_Config, New_User_Config : Alice_User_Config.User_Config_Type;

      Config_File_Exists : constant Boolean :=
        Alice_User_Config.Has_User_Config_File (Report_Error => False);

      Login_Changed : Boolean := False;

      Success : Boolean;
   begin
      if Config_File_Exists then
         Log.Warning
           ("User configuration file already exists, " &
            "do you want to continue?");
         CLIC.User_Input.Continue_Or_Abort;
         Old_User_Config := Alice_User_Config.Read_From_File;
         Log.Debug ("Old_User_Config" & Old_User_Config'Image);
      end if;

      Log.Always ("Retrieving information from GitHub and git");
      Success := New_User_Config.Get_Info_From_Token (Token);

      if not Success then
         return;
      end if;

      Log.Debug ("New_User_Config" & New_User_Config'Image);

      if Config_File_Exists then
         Log.Debug ("Keep SPDX_Id from old config file, if any");
         Success :=
           New_User_Config.Set_SPDX
             (Old_User_Config.SPDX, Report_Error => False);
         if Success then
            Log.Detail
              ("keeping SPDX_Id '" & To_String (New_User_Config.SPDX) &
               "' from old config file");
         else
            Log.Warning
              ("Could not save current SPDX Id '" &
               To_String (Old_User_Config.SPDX) & "': invalid");
            Log.Warning ("Using default SPDX Id");
            Log.Warning ("Set a different one with 'alice config --license'");
            Log.Always ("");
         end if;
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
            To_String (Old_User_Config.Login) & "'");
         Log.Error
           ("  • provided token is for user login '" &
            To_String (New_User_Config.Login) & "'");
         Log.Error
           ("Overwriting the configuration file with a different login can cause serious problems,");
         Log.Error
           ("both in your locally cloned repositories and in Alice participation.");
         Log.Error
           ("Answer 'Yes' ONLY if you are completely sure about what you are doing.");
         Log.Always ("");
         pragma Style_Checks (on);
      end if;

      Log.Always ("New configuration file contents is:");
      New_User_Config.Show;
      Log.Always ("");

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
            return;
         end if;
         Log.Always ("");
      end;

      if Config_File_Exists then
         Log.Debug ("make a backup copy of user config file");
         declare
            User_Config_File : constant String :=
              Alice_User_Config.Config_Directory &
              GNAT.Directory_Operations.Dir_Separator &
              Alice_User_Config.User_Config_File;

            Backup_Config_File : constant String :=
              User_Config_File & ".backup";

            Success : Boolean;
         begin
            GNAT.OS_Lib.Copy_File
              (User_Config_File, Backup_Config_File, Success,
               GNAT.OS_Lib.Overwrite);
            if not Success then
               Log.Warning ("Could not make a backup copy of the config file");
            end if;
         end;
      end if;

      if New_User_Config.Write_To_File then
         Log.Always ("New user configuration file saved");
      else
         Log.Error ("Could not save the new user configuration file");
      end if;
   end Execute_Token;

   ---------------------
   -- Execute_License --
   ---------------------

   procedure Execute_License (SPDX_Id : Unbounded_String) is
   begin
      null;
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
      end if;

      if Actions > 1 then
         Log.Error ("Specify only one subcommand");
         return;
      end if;

      if Cmd.Show then
         Execute_Show;
      elsif Cmd.Refresh then
         Execute_Refresh;
      elsif Cmd.Token then
         if Args_Length < 1 then
            Log.Error ("Too few arguments, <github_token> required");
            return;
         end if;

         if Args_Length > 1 then
            Log.Error ("Provide only one <github_token> argument");
            return;
         end if;

         Execute_Token (To_Unbounded_String (Args.First_Element));
      elsif Cmd.License then

         if Args_Length < 1 then
            Log.Error ("Too few arguments, <spdx_id> required");
            return;
         end if;

         if Args_Length > 1 then
            Log.Error ("Provide only one <spdx_id> argument");
            return;
         end if;

         Execute_License (To_Unbounded_String (Args.First_Element));
      else
         Log.Error ("subcommand not found (?)");
      end if;
   end Execute;

end Alice_Cmd.Setup.Config;
