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

   type Object is interface and Alice.IFace.Object;

   type Object_Access is access all Object'Class;

   type Result (Status : Alice.Result.Status_Type) is
     new Alice.Result.Object with private;

   function Get_User_Name (Self : in out Object) return String is abstract;
   --  Returns the user name associated with the VCS profile.

   function Get_User_Email (Self : in out Object) return String is abstract;
   --  Returns the email address associated with the VCS profile.

   function Get_User_Login (Self : in out Object) return String is abstract;
   --  Returns the login name associated with the VCS profile.

   function Get_User_Token (Self : in out Object) return String is abstract;
   --  Returns the token associated with the VCS profile. This token is used
   --  for authentication with the VCS service.

   function Get_SPDX_ID (Self : in out Object) return String is abstract;
   --  Returns the SPDX license identifier associated with the VCS profile.
   --  Each Alice member can have a specific license identifier that applies
   --  to their contributions or repositories.

   procedure Set_SPDX_ID (Self : in out Object; SPDX : String) is abstract;
   --  Sets the SPDX license identifier for the VCS profile. This allows Alice
   --  members to specify the license under which their contributions or
   --  repositories are shared. The SPDX identifier should be a valid SPDX
   --  license identifier, such as "MIT", "GPL-3.0", or "Apache-2.0".

   function Save_To_File
     (Self : in out Object; File : String) return Alice.Result.Object'Class
   is abstract;
   --  Saves the VCS profile to a specified file. The implementation should
   --  write the profile information to the file in a format that can be
   --  easily read and parsed later. If the file cannot be written or the
   --  profile cannot be saved, it should return an error result.

private

   type Result (Status : Alice.Result.Status_Type) is
     new Alice.Result.Object (Status)
   with record
      case Status is
         when Alice.Result.Success =>
            Profile : Alice.IFace.VCS.Profile.Object_Access;

         when Alice.Result.Error =>
            null;
      end case;
   end record;

end Alice.IFace.VCS.Profile;
