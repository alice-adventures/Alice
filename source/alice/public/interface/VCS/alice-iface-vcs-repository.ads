-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package provides the interface for Version Control System (VCS)
--  repositories. It defines the basic operations that any VCS repository
--  implementation should support, mainly cloning, pulling, pushing, and
--  others.

with Alice.Result;

package Alice.IFace.VCS.Repository is

   type Object is interface and Alice.IFace.Object;

   type Object_Access is not null access all Object'Class;

   function Create
     (Self     : in out Object;
      Name     : String := "";
      URL      : String := "";
      Provider : String := "") return Alice.Result.Object'Class
   is abstract;
   --  Creates a new repository object with the specified name, URL, and
   --  provider. The implementation should return the created repository
   --  object or an error if the creation fails. The name is typically the
   --  local directory name where the repository will be cloned, the URL is
   --  the remote URL of the repository, and the provider is the service or
   --  platform where the repository is hosted (e.g., GitHub, GitLab,
   --  Bitbucket).

   function Get_Repository_From_CWD
     (Self : in out Object) return Alice.Result.Object'Class
   is abstract;
   --  Returns the repository object for the current working directory. The
   --  implementation should return the repository object if the current
   --  working directory is a valid repository, or an error if it is not. This
   --  is useful for operations that need to determine the repository context
   --  based on the current working directory, such as cloning, pulling, or
   --  pushing changes.

   function CWD_Is_Clone_Of (Self : in out Object) return Boolean is abstract;
   --  Checks if the current working directory is a clone of the specified
   --  repository. The implementation should return True if the current
   --  working directory is a clone of the specified repository, or False if
   --  it is not. This is useful for determining if the user is currently
   --  working within a cloned repository and can perform operations like
   --  pull, push, or commit. The Repository parameter is typically the remote
   --  URL or name of the repository to check against the current working
   --  directory.

   function Clone
     (Self : in out Object; Directory : String := ""; Branch : String := "")
      return Alice.Result.Object'Class
   is abstract;
   --  Clones the repository to a local directory. The implementation should
   --  return the result of the clone operation, which may include the local
   --  path of the cloned repository or an error if the clone operation fails.

   function Switch
     (Self : in out Object; Branch : String) return Alice.Result.Object'Class
   is abstract;
   --  Switches to the specified branch in the repository. The implementation
   --  should return the result of the switch operation, which may include the
   --  updated state of the repository or an error if the switch operation
   --  fails.

   function Pull (Self : in out Object) return Alice.Result.Object'Class
   is abstract;
   --  Pulls the latest changes from the remote repository. The implementation
   --  should return the result of the pull operation, which may include the
   --  updated state of the repository or an error if the pull operation
   --  fails.

   function Push (Self : in out Object) return Alice.Result.Object'Class
   is abstract;
   --  Pushes local changes to the remote repository. The implementation
   --  should return the result of the push operation, which may include the
   --  updated state of the repository or an error if the push operation
   --  fails.

   function Commit
     (Self : in out Object; Message : String) return Alice.Result.Object'Class
   is abstract;
   --  Commits local changes with the specified message. The implementation
   --  should return the result of the commit operation, which may include the
   --  updated state of the repository or an error if the commit operation
   --  fails.

   function Get_Branches
     (Self : in out Object) return Alice.Result.Object'Class
   is abstract;
   --  Returns a list of branches in the repository. The implementation should
   --  return a list of branch names or an error if the operation fails.

   function Get_Current_Branch
     (Self : in out Object) return Alice.Result.Object'Class
   is abstract;
   --  Returns the name of the current branch in the repository. The
   --  implementation should return the name of the current branch or an error
   --  if the operation fails.

   function Get_Status (Self : in out Object) return Alice.Result.Object'Class
   is abstract;
   --  Returns the status of the repository, including modified, added, or
   --  deleted files. The implementation should return a status object that
   --  contains information about the current state of the repository or an
   --  error if the operation fails.

   function Get_Log (Self : in out Object) return Alice.Result.Object'Class
   is abstract;
   --  Returns the commit log of the repository. The implementation should
   --  return a list of commit objects or an error if the operation fails.

   function Get_Name (Self : in out Object) return Alice.Result.Object'Class
   is abstract;
   --  Returns the name of the repository. The implementation should return
   --  the name of the repository or an error if the operation fails. The name
   --  is typically derived from the repository's remote URL or the local
   --  directory name where the repository is cloned.

   function Get_Description
     (Self : in out Object) return Alice.Result.Object'Class
   is abstract;
   --  Returns the description of the repository. The implementation should
   --  return the description of the repository or an error if the operation
   --  fails. The description is typically a brief summary of the repository's
   --  purpose or content, which may be stored in the repository's metadata or
   --  configuration files.

   function Get_Owner (Self : in out Object) return Alice.Result.Object'Class
   is abstract;
   --  Returns the owner of the repository. The implementation should return
   --  the owner of the repository or an error if the operation fails. The
   --  owner is typically the user or organization that created the repository
   --  and is often associated with the repository's remote URL or metadata.

   function Get_URL (Self : in out Object) return Alice.Result.Object'Class
   is abstract;
   --  Returns the URL of the repository. The implementation should return the
   --  URL of the repository or an error if the operation fails. This is
   --  typically the same as the remote URL but can be different in some
   --  cases, such as when the repository is hosted on a platform that
   --  provides a specific URL for the repository.

   function Get_Clone_URL
     (Self : in out Object) return Alice.Result.Object'Class
   is abstract;
   --  Returns the clone URL of the repository. The implementation should
   --  return the URL that can be used to clone the repository, which may be
   --  different from the remote URL. This is typically the URL that includes
   --  the protocol (e.g., HTTPS or SSH) and the repository path, allowing
   --  users to clone the repository using standard VCS commands.

   function Get_Provider
     (Self : in out Object) return Alice.Result.Object'Class
   is abstract;
   --  Returns the provider of the repository. The implementation should
   --  return the provider of the repository or an error if the operation
   --  fails. The provider is typically the service or platform where the
   --  repository is hosted, such as GitHub, GitLab, Bitbucket, or self-hosted
   --  Git servers. This information can be useful for determining the
   --  capabilities and features available for the repository, as different
   --  providers may offer different APIs, authentication methods, and
   --  integration options.

end Alice.IFace.VCS.Repository;
