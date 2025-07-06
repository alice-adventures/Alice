-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package provides the interface for Version Control System (VCS) API
--  operations in the Alice application. It defines the basic operations that
--  any VCS API implementation should support, such as creating repositories,
--  managing branches, and handling commits.
--
--  Operations in this interface are designed to be implemented by specific VCS
--  API providers, allowing for flexibility in how different VCS systems are
--  integrated into the Alice application.
--
--  This package is intended to be used in conjunction with the VCS_Profile
--  interface, which defines the basic operations for retrieving user data
--  fields from VCS profiles.
--
--  The interface provides methods for User and Owner profiles. Consider that
--  User repositories are read-only, and Owned ones are read-write. Thus, only
--  Owned repositories can be changed by the current Alice member.
--
--  Each method is expected to return an `Alice.Result.Object'Class`, which
--  encapsulates the result of the operation, including any errors that may
--  occur during the process.

with Alice.Result;
with Alice.VCS.Profile;

package Alice.IFace.VCS.API is

   type Object is interface and Alice.IFace.Object;

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

   function Get_Owner_Repository
     (Self    : in out Object;
      Profile : Alice.VCS.Profile.Object'Class;
      Name    : String) return Alice.Result.Object'Class
   is abstract;
   --  Retrieves the owner repository by Name. The implementation should
   --  return the owner repository associated with the given name. If the
   --  owner repository does not exist, it should return an error result.

   function Create_Owner_Repository
     (Self        : in out Object;
      Profile     : Alice.VCS.Profile.Object'Class;
      Name        : String;
      Description : String) return Alice.Result.Object'Class
   is abstract;
   --  Creates a new owner repository with the given name and description. The
   --  implementation should create the repository and return the result.

   function Create_Owner_Repository_From_Template
     (Self        : in out Object;
      Profile     : Alice.VCS.Profile.Object'Class;
      Template    : String;
      Name        : String;
      Description : String) return Alice.Result.Object'Class
   is abstract;
   --  Creates a new owner repository using a template. The implementation
   --  should create the repository from the specified template and return the
   --  result. If the template does not exist or cannot be used, it should
   --  return an error result.

   --  #FIXME - Needed?
   --  function Get_Owner_Repository_List
   --    (Self : in out Object; Profile : Alice.VCS.Profile.Object'Class)
   --     return Alice.Result.Object'Class
   --  is abstract;
   --  Retrieves a list of owner repositories associated with the profile. If
   --  the profile does not have any owner repositories, it should return an
   --  empty list.

end Alice.IFace.VCS.API;
