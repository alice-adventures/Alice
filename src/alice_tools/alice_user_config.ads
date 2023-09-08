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

   Empty_Config : constant User_Config_Type;

   --!pp off
   function Author (User_Config : User_Config_Type) return String;
   function Email  (User_Config : User_Config_Type) return String;
   function Login  (User_Config : User_Config_Type) return String;
   function Token  (User_Config : User_Config_Type) return String;
   function SPDX   (User_Config : User_Config_Type) return String;
   --!pp on

   function Get_Info_From_Token
     (User_Config : in out User_Config_Type; Token : String) return Boolean;
   --  Set the GitHub personal access token associated to the Participant
   --  GitHub' account and all extracts all the necessary information.

   function Set_SPDX
     (User_Config  : in out User_Config_Type; SPDX_Id : String;
      Report_Error :        Boolean := True) return Boolean;
   --  Set the SPDX License Id. If SPDX is not a valid SPDX Id, reports an
   --  error (optionally) and returns False.

   function Has_User_Config_File
     (Report_Error : Boolean := True) return Boolean;
   --  Check that the user config file exists.

   function Read_From_File
     (User_Config : in out User_Config_Type; Report_Error : Boolean := True)
      return Boolean;
   --  Read user configuration from the user config file.

   function Write_To_File (User_Config : User_Config_Type) return Boolean;
   --  Write user configuration to the user config file.

   function Get_Current_User return User_Config_Type;
   --  Return the currently configured user, reading configuration from file
   --  when needed.

   procedure Show (User_Config : User_Config_Type);
   --  Write user configuration contents to the console.

private

   type User_Config_Type is record
      GitHub_Login : Unbounded_String := Null_Unbounded_String;
      --  From GitHub

      GitHub_Token : Unbounded_String := Null_Unbounded_String;
      --  From GitHub

      User_Email : Unbounded_String := Null_Unbounded_String;
      --  From GitHub, or else from git config

      User_Name : Unbounded_String := Null_Unbounded_String;
      --  From GitHub, or else from git config

      SPDX_Id : Unbounded_String := Null_Unbounded_String;
      --  SPDX_Id : Unbounded_String := To_Unbounded_String ("MIT");
      --  given by user, default is 'MIT'

      Valid_Token : Boolean := False;
   end record;

   Empty_Config : constant User_Config_Type :=
     (Null_Unbounded_String, Null_Unbounded_String, Null_Unbounded_String,
      Null_Unbounded_String, Null_Unbounded_String, False);

   function Get_Info_From_GitHub_Token
     (User_Config : in out User_Config_Type) return Boolean;
   --  If not null, get login, name and email from the GitHub user account
   --  associated to the token. Return False if the token is not set or it is
   --  not associated to a valid GitHub user account. It must be a user
   --  account, not an organization account.

   function Get_Info_From_Git_Config
     (User_Config : in out User_Config_Type) return Boolean;
   --  If not null, get name and email from 'git config -l'. Return False if
   --  name or email are not set in git config.

end Alice_User_Config;
