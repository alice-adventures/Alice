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

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Cmd : in out Cmd_Type; Args : AAA.Strings.Vector)
   is
      Old_User_Config, New_User_Config : Alice_User_Config.User_Config_Type;

      Config_File_Exists : constant Boolean :=
        Alice_User_Config.Has_User_Config_File;

      Success     : Boolean;
      Args_Length : constant Natural := Natural (Args.Length);
   begin

      if Args_Length < 1 then
         Log.Error ("Too few arguments, TOKEN required");
         return;
      end if;

      if Args_Length > 1 then
         Log.Error ("Please provide only one TOKEN");
         return;
      end if;

      if Config_File_Exists then
         Log.Warning
           ("User configuration file already exists, " &
            "do you want to continue?");
         CLIC.User_Input.Continue_Or_Abort;
         Old_User_Config := Alice_User_Config.Read_From_File;
         Log.Debug ("Old_User_Config" & Old_User_Config'Image);
      end if;

      Success :=
        New_User_Config.Get_Info_From_Token
          (To_Unbounded_String (Args.First_Element));

      if not Success then
         return;
      end if;

      Log.Debug ("New_User_Config" & New_User_Config'Image);

      if Config_File_Exists
        and then Old_User_Config.Login /= New_User_Config.Login
      then
         pragma Style_Checks (off);
         Log.Error
           ("Token provided is associated to a different GitHub account");
         Log.Error
           ("Currently configured token is for user login '" &
            To_String (Old_User_Config.Login) & "'");
         Log.Error
           ("Provided token is for user login '" &
            To_String (New_User_Config.Login) & "'");
         Log.Error
           ("Overwriting the configuration file can cause serious problem in your locally cloned repositories and in Alice participation.");
         pragma Style_Checks (on);
      end if;

      Log.Always ("");
      Log.Always ("New configuration file is:");
      Log.Always ("   login : " & To_String (New_User_Config.Login));
      Log.Always ("   name  : " & To_String (New_User_Config.Author));
      Log.Always ("   email : " & To_String (New_User_Config.Email));
      Log.Always ("   token : " & To_String (New_User_Config.Token));

      declare
         Answer : CLIC.User_Input.Answer_Kind;
      begin
         Answer :=
           CLIC.User_Input.Query
             ("Do you want to continue?", Valid => [True, True, False],
              Default                           =>
                (if Old_User_Config.Login /= New_User_Config.Login then
                   CLIC.User_Input.No
                 else CLIC.User_Input.Yes));
         if Answer = CLIC.User_Input.No then
            return;
         end if;
      end;

      if Config_File_Exists then
         --  make a backup copy of user config file
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
         Log.Info ("New user configuration file saved");
      else
         Log.Error ("Could not save the new user configuration file");
      end if;

   end Execute;

end Alice_Cmd.Setup.Config;
