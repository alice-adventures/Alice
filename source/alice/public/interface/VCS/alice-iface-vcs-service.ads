-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package provides the interface for Version Control System (VCS)
--  service operations in the Alice application. It defines the basic
--  operations that any VCS API implementation should support, such as
--  accessing user profiles, retrieving repositories information and creating
--  repositories.
--
--  Operations in this interface are designed to be implemented by specific
--  VCS service providers, allowing for flexibility in how different VCS
--  systems are integrated into the Alice application.
--
--  This package is intended to be used in conjunction with the VCS_Profile
--  interface, which defines the basic operations for retrieving user data
--  fields from VCS profiles.
--
--  The interface provides methods for User and Member profiles. Consider that
--  User repositories are read-only, and member' ones are read-write. Thus,
--  only member repositories can be changed by the current Alice member.
--
--  Each method is expected to return an `Alice.Result.Object'Class`, which
--  encapsulates the result of the operation, including any errors that may
--  occur during the process.

with Alice.Result;
with Alice.VCS.Profile.Result;

package Alice.IFace.VCS.Service is

   type Object is interface and Alice.IFace.Object;

   type Object_Access is not null access all Object'Class;

   function Get_Member_Profile_From_Token
     (Self : in out Object; Token : String)
      return Alice.VCS.Profile.Result.Object'Class
   is abstract;
   --  Retrieves a VCS profile using the provided token. The implementation
   --  should return the profile associated with the given token by connecting
   --  to a remote service (e.g., GitHub). If the token is invalid or the
   --  profile does not exist, it should return an error result.

   function Get_Member_Profile_From_VCS_Config_File
     (Self : in out Object; Token : String)
      return Alice.VCS.Profile.Result.Object'Class
   is abstract;
   --  Retrieves a VCS profile using the provided token from a VCS
   --  configuration file (e.g. '~/.gitconfig'). The implementation should
   --  read the configuration file and extract the profile information
   --  associated with the given token. If the token is invalid or the profile
   --  does not exist, it should return an error result.

   function Get_Member_Profile_From_Alice_Config_File
     (Self : in out Object; File : String)
      return Alice.VCS.Profile.Result.Object'Class
   is abstract;
   --  Retrieves a VCS profile from the Alice configuration file. The
   --  implementation should read the specified configuration file and extract
   --  the profile information. If the file does not exist or the profile is
   --  not found, it should return an error result.

   function Get_Member_Repository
     (Self    : in out Object;
      Profile : Alice.VCS.Profile.Object'Class;
      Name    : String) return Alice.Result.Object'Class
   is abstract;
   --  Retrieves the member repository by Name. The implementation should
   --  return the member repository associated with the given name. If the
   --  member repository does not exist, it should return an error result.

   function Create_Member_Repository
     (Self        : in out Object;
      Profile     : Alice.VCS.Profile.Object'Class;
      Name        : String;
      Description : String) return Alice.Result.Object'Class
   is abstract;
   --  Creates a new member repository with the given name and description.
   --  The implementation should create the repository and return the result.

   function Create_Member_Repository_From_Template
     (Self        : in out Object;
      Profile     : Alice.VCS.Profile.Object'Class;
      Template    : String;
      Name        : String;
      Description : String) return Alice.Result.Object'Class
   is abstract;
   --  Creates a new member repository using a template. The implementation
   --  should create the repository from the specified template and return the
   --  result. If the template does not exist or cannot be used, it should
   --  return an error result.

   --  #FIXME - Needed?
   --  function Get_Member_Repository_List
   --    (Self : in out Object; Profile : Alice.VCS.Profile.Object'Class)
   --     return Alice.Result.Object'Class
   --  is abstract;
   --  Retrieves a list of member repositories associated with the profile. If
   --  the profile does not have any member repositories, it should return an
   --  empty list.

   function Get_User
     (Self : in out Object; Name : String) return Alice.Result.Object'Class
   is abstract;
   --  Retrieves a user profile by name. The implementation should return the
   --  user profile associated with the given name. If the user does not
   --  exist, it should return an error result.

   function Get_User_Repository
     (Self : in out Object; Token : String) return Alice.Result.Object'Class
   is abstract;
   --  Retrieves a repository profile by Name. The implementation should
   --  return the repository associated with the given name. If the repository
   --  does not exist, it should return an error result.

   --  #FIXME - Needed?
   --  function Get_User_Repository_List
   --    (Self : in out Object; Name : String) return Alice.Result.Object'Class
   --  is abstract;
   --  Retrieves a list of repositories associated with the user. If the user
   --  does not have any repositories, it should return an empty list.

end Alice.IFace.VCS.Service;
