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

with Alice_User_Config;

package body Alice_Cmd.Setup.Config is

   package Log renames Simple_Logging;

   use all type Alice_User_Config.User_Config_Type;
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
      User_Config : Alice_User_Config.User_Config_Type;
   begin
      if not Alice_User_Config.Has_User_Config_File then
         return;
      end if;

      if not User_Config.Read_From_File then
         return;
      end if;

      Log.Info ("User configuration file contents is:");
      User_Config.Show;
   end Execute_Show;

   ---------------------
   -- Execute_Refresh --
   ---------------------

   procedure Execute_Refresh is
      Success         : Boolean;
      New_User_Config : Alice_User_Config.User_Config_Type;
      Old_User_Config : Alice_User_Config.User_Config_Type;
   begin
      if not Alice_User_Config.Has_User_Config_File then
         return;
      end if;

      if not Old_User_Config.Read_From_File then
         return;
      end if;

      Success := New_User_Config.Get_Info_From_Token (Old_User_Config.Token);

      if Success then
         Log.Debug
           ("New_User_Config.Get_Info_From_Token =" & New_User_Config'Image);
         if Old_User_Config.Login = New_User_Config.Login then
            Success :=
              New_User_Config.Set_SPDX
                (Old_User_Config.SPDX, Report_Error => False);
            if not Success then
               Log.Warning
                 ("Could not keep invalid SPDX Id '" & Old_User_Config.SPDX &
                  "', default 'MIT' applied");
            end if;

            if New_User_Config /= Old_User_Config then
               Success := New_User_Config.Write_To_File;
               Log.Info ("New user config file saved");
               New_User_Config.Show;
            else
               Log.Info ("No changes detected, config file unchanged");
            end if;
         else
            Log.Error
              ("Cannot change GitHub login, " &
               "run 'alice config --token' instead");
         end if;
      else
         Log.Error ("Run 'alice config --token' with a valid token");
      end if;

   end Execute_Refresh;

   -------------------
   -- Execute_Token --
   -------------------

   procedure Execute_Token (Token : String) is separate;

   ---------------------
   -- Execute_License --
   ---------------------

   procedure Execute_License (SPDX_Id : String) is
      Success     : Boolean := False;
      User_Config : Alice_User_Config.User_Config_Type;
   begin
      if not Alice_User_Config.Has_User_Config_File then
         return;
      end if;

      if not User_Config.Read_From_File then
         return;
      end if;

      Log.Debug ("User_Config.Read_From_File =" & User_Config'Image);

      if User_Config.SPDX = SPDX_Id then
         Log.Warning ("Same SPDX Id specified, nothing changed");
      else
         Success := User_Config.Set_SPDX (SPDX_Id);
         Log.Debug ("User_Config.Set_SPDX =" & User_Config'Image);

         if Success then
            Success := User_Config.Write_To_File;
            Log.Info
              ("New SPDX license Id '" & SPDX_Id &
               "' will be applied from now on");
         else
            Log.Error ("Choose a valid Id from https://spdx.org/licenses");
         end if;
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
