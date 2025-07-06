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

with Alice.Result;

package Alice.VCS.Profile is

   type Object is new Alice.Controlled with private;

   type Object_Access is access Object;

   function Get_User_Name (Self : Object) return String
   with Inline;

   function Get_User_Email (Self : Object) return String
   with Inline;

   function Get_User_Login (Self : Object) return String
   with Inline;

   function Get_User_Avatar (Self : Object) return String
   with Inline;

   function Get_User_Token (Self : Object) return String
   with Inline;

   --  #FIXME - Get the VCS Provider with which the profile is associated. It
   --  is used to determine the specific VCS provider required to retrieve the
   --  profile form the token.
   --
   --  function Get_VCS_Provider (Self : Object) return
   --  Alice.VCS.Provider.Result.Object with Inline;

   function Get_SPDX_Id (Self : Object) return String
   with Inline;

   procedure Set_SPDX_Id (Self : in out Object; SPDX_Id : String);
   --  Set_SPDX_Id sets the SPDX ID for the VCS profile object. It updates the
   --  User_SPDX_Id field with the provided SPDX string. This allows the
   --  profile to include the SPDX ID, which is useful for licensing and
   --  compliance purposes. If the SPDX string is not valid, a default SPDX ID
   --  will be set, depending on the default SPDX ID defined in the
   --  implementation.

   function Save_To_File
     (Self : Object; File : String) return Alice.Result.Object'Class;
   --  Saves the VCS profile to a file. The implementation should write the
   --  profile information to the specified file in a format that can be
   --  easily read back later.

private

   type Object is new Alice.Controlled with record
      User_Name   : Alice.UString := Alice.UStr ("");
      User_Email  : Alice.UString := Alice.UStr ("");
      User_Login  : Alice.UString := Alice.UStr ("");
      User_Avatar : Alice.UString := Alice.UStr ("");
      User_Token  : Alice.UString := Alice.UStr ("");
      SPDX_Id     : Alice.UString := Alice.UStr ("");
   end record;

end Alice.VCS.Profile;
