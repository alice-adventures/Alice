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

with Alice.Result;

package Alice.IFace.VCS.Provider is

   type Object is interface;

   function Get_Profile_From_Token
     (Self : in out Object; Token : String) return Alice.Result.Object'Class
   is abstract;

   function Get_Profile_From_VCS_Config_File
     (Self : in out Object; Token : String) return Alice.Result.Object'Class
   is abstract;

   function Get_Profile_From_Config_File
     (Self : in out Object; File : String) return Alice.Result.Object'Class
   is abstract;

end Alice.IFace.VCS.Provider;
