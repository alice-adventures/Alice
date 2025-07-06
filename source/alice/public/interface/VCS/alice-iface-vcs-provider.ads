-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package provides the interface for Version Control System (VCS)
--  providers in the Alice application. It defines the basic operations that
--  any VCS implementation should support to access user profiles and
--  credentials.
--
--  VCS providers are, for example, Git, Mercurial, or any other version
--  control system that can be integrated into the Alice application. A
--  provider can be also different Git service, such as GitHub, GitLab, or
--  Bitbucket.

with Alice.VCS.Profile.Result;

package Alice.IFace.VCS.Provider is

   type Object is interface and Alice.IFace.Object;

   function Get_Profile_From_Token
     (Self : in out Object; Token : String)
      return Alice.VCS.Profile.Result.Object'Class
   is abstract;
   --  Retrieves a VCS profile using the provided token. The implementation
   --  should return the profile associated with the given token by connecting
   --  to a remote service (e.g., GitHub). If the token is invalid or the
   --  profile does not exist, it should return an error result.

   function Get_Profile_From_VCS_Config_File
     (Self : in out Object; Token : String)
      return Alice.VCS.Profile.Result.Object'Class
   is abstract;
   --  Retrieves a VCS profile using the provided token from a VCS
   --  configuration file (e.g. '~/.gitconfig'). The implementation should
   --  read the configuration file and extract the profile information
   --  associated with the given token. If the token is invalid or the profile
   --  does not exist, it should return an error result.

   function Get_Profile_From_Alice_Config_File
     (Self : in out Object; File : String)
      return Alice.VCS.Profile.Result.Object'Class
   is abstract;
   --  Retrieves a VCS profile from the Alice configuration file. The
   --  implementation should read the specified configuration file and extract
   --  the profile information. If the file does not exist or the profile is
   --  not found, it should return an error result.

end Alice.IFace.VCS.Provider;
