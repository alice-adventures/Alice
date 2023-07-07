-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Alice_User_Config is

   Config_Directory : constant String := "config";
   User_Config_File : constant String := "user.toml";

   type User_Config_Type is private;

   function Author (User_Config : User_Config_Type) return Unbounded_String;
   function Login (User_Config : User_Config_Type) return Unbounded_String;
   function Email (User_Config : User_Config_Type) return Unbounded_String;
   function Token (User_Config : User_Config_Type) return Unbounded_String;

   procedure Token
     (User_Config : in out User_Config_Type; Token : Unbounded_String);

   function Get_Info_From_Github
     (User_Config : in out User_Config_Type) return Boolean;
   --  If not null, get login, name and email from the Github user account
   --  associated to the token. Return False if the token is not set or it is
   --  not associated to a valid Github account.

   function Get_Info_From_Git_Config
     (User_Config : in out User_Config_Type) return Boolean;
   --  If not null, get name and email from 'git config -l'. Return False if
   --  name or email are not set in git config.

   function Has_User_Config_File return Boolean;
   --  Check that the user config file exists.

   function Check_User_Config_File return Boolean;
   --  Check that the contents of user config file is correct.

   function Read_User_Config_File return User_Config_Type;
   --  Read user configuration from the user config file.

   function Write_User_Config_File
     (User_Config : User_Config_Type) return Boolean;
   --  Write user configuration to the user config file.

private

   type User_Config_Type is record
      Github_Login : Unbounded_String; -- from Github
      Github_Token : Unbounded_String; -- from Github
      User_Email   : Unbounded_String; -- from Github, or else from git config
      User_Name    : Unbounded_String; -- From Github, or else from git config
   end record;

end Alice_User_Config;
