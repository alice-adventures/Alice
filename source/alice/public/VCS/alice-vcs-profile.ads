-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package provides the Profile type for operations related to VCS
--  profiles in the Alice application. It defines a tagged record that
--  encapsulates the user profile information, including name, email, login,
--  avatar, token, and SPDX ID. It also provides functions to access and
--  manipulate these fields, as well as a procedure to save the profile to a
--  file.

with Ada.Strings.Text_Buffers;

with Alice.Result;
with Alice.VCS.Service;

package Alice.VCS.Profile is

   type Object is new Alice.Controlled with private;

   type Object_Access is access Object;

   function Create_Profile
     (Service    : Alice.VCS.Service.Name.Enum;
      Token      : Alice.UString;
      Login      : Alice.UString;
      Avatar_URL : Alice.UString := Alice.Null_UString;
      Name       : Alice.UString := Alice.Null_UString;
      Email      : Alice.UString := Alice.Null_UString;
      SPDX_Id    : Alice.UString := Alice.Null_UString) return Object_Access;

   function Get_Service (Self : Object) return Alice.VCS.Service.Name.Enum
   with Inline;

   function Get_Token (Self : Object) return String
   with Inline;

   function Get_Login (Self : Object) return String
   with Inline;

   function Get_Avatar_URL (Self : Object) return String
   with Inline;

   function Get_Name (Self : Object) return String
   with Inline;

   function Get_Email (Self : Object) return String
   with Inline;

   function Get_SPDX_Id (Self : Object) return String
   with Inline;

   procedure Set_SPDX_Id (Self : in out Object; SPDX_Id : String);
   --  Set_SPDX_Id sets the SPDX ID for the VCS profile object. It updates the
   --  User_SPDX_Id field with the provided SPDX string. This allows the
   --  profile to include the SPDX ID, which is useful for licensing and
   --  compliance purposes. If the SPDX string is not valid, a default SPDX ID
   --  will be set, depending on the default SPDX ID defined in the
   --  implementation.

   function Load_From_File
     (Self : in out Object; File : String) return Alice.Result.Object'Class;
   --  Loads the profile from a specified file. It reads the file and extracts
   --  the profile information, returning an object that contains the profile
   --  details. If the file does not exist or the profile is not found, it
   --  returns an error result.

   function Save_To_File
     (Self : in out Object; File : String) return Alice.Result.Object'Class;
   --  Saves the VCS profile to a file. The implementation should write the
   --  profile information to the specified file in a format that can be
   --  easily read back later.

private

   procedure Profile_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Object);

   type Object is new Alice.Controlled with record
      Service    : Alice.UString := Alice.Null_UString;
      Token      : Alice.UString := Alice.Null_UString;
      Login      : Alice.UString := Alice.Null_UString;
      Avatar_URL : Alice.UString := Alice.Null_UString;
      Name       : Alice.UString := Alice.Null_UString;
      Email      : Alice.UString := Alice.Null_UString;
      SPDX_Id    : Alice.UString := Alice.Null_UString;
   end record
   with Put_Image => Profile_Image;

end Alice.VCS.Profile;
