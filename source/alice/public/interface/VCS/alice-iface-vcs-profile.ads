-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package provides the interface for Version Control System (VCS)
--  profiles. It defines the basic operations that any VCS profile
--  implementation should support, mainly retrieving user data fields.

with Alice.Result;

package Alice.IFace.VCS.Profile is

   type Object is interface;

   function Get_User_Name (Self : in out Object) return Alice.UString
   is abstract;
   --  Returns the user name associated with the VCS profile.

   function Get_User_Email (Self : in out Object) return Alice.UString
   is abstract;
   --  Returns the email address associated with the VCS profile.

   function Get_User_Login (Self : in out Object) return Alice.UString
   is abstract;
   --  Returns the login name associated with the VCS profile.

   function Get_User_Token (Self : in out Object) return Alice.UString
   is abstract;
   --  Returns the token associated with the VCS profile. This token is used
   --  for authentication with the VCS service.

   function Get_User_SPDX (Self : in out Object) return Alice.UString
   is abstract;
   --  Returns the SPDX license identifier associated with the VCS profile.

   function Save_To_File
     (Self : in out Object; File : String) return Alice.Result.Object'Class
   is abstract;

end Alice.IFace.VCS.Profile;
