-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package GitHub.Profile is

   package Error is
      Load_Profile : exception;
   end Error;
   GitHub_Profile_Error : exception;

   type Profile_Type is private;
   Empty_Profile : constant Profile_Type;

   Key_Email   : constant String := "email";
   Key_Login   : constant String := "login";
   Key_Name    : constant String := "name";
   Key_Token   : constant String := "token";
   Key_SPDX_Id : constant String := "spdx_id";

   --!pp off
   function Name  (Profile : Profile_Type) return String;
   function Email (Profile : Profile_Type) return String;
   function Login (Profile : Profile_Type) return String;
   function Token (Profile : Profile_Type) return String;
   function SPDX  (Profile : Profile_Type) return String;
   --!pp on

   procedure Load_From_GitHub_Token
     (Profile : in out Profile_Type; Token : String);
   --  Retrieves all possible values from a valid token associated to the
   --  Participant GitHub account.

   procedure Load_From_Git_Config (Profile : in out Profile_Type);
   --  Retrieve all possible values from the git configuration of the
   --  Participant.

   procedure Load_From_File (Profile : in out Profile_Type);
   --  Read the user profile from the user profile file.

   procedure Save_To_File (Profile : Profile_Type);
   --  Write the user profile to the user profile file.

   procedure Set_SPDX (Profile : in out Profile_Type; SPDX_Id : String);
   --  Set the SPDX License Id. If SPDX is not a valid SPDX Id, reports an
   --  error (optionally) and returns False.

   function Is_Valid (Profile : Profile_Type) return Boolean;
   --  Return True if the profile has a valid GitHub Token.

private

   type Profile_Type is record
      User_Name : Unbounded_String := Null_Unbounded_String;
      --  From GitHub, or else from git config

      User_Email : Unbounded_String := Null_Unbounded_String;
      --  From GitHub, or else from git config

      GitHub_Login : Unbounded_String := Null_Unbounded_String;
      --  From GitHub

      GitHub_Token : Unbounded_String := Null_Unbounded_String;
      --  From GitHub

      SPDX_Id : Unbounded_String := Null_Unbounded_String;
      --  SPDX_Id : Unbounded_String := To_Unbounded_String ("MIT");
      --  given by user, default is 'MIT'

      Valid_Token : Boolean := False;
   end record;

   Empty_Profile : constant Profile_Type :=
     (Null_Unbounded_String, Null_Unbounded_String, Null_Unbounded_String,
      Null_Unbounded_String, Null_Unbounded_String, False);

end GitHub.Profile;
