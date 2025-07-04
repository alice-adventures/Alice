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

   function Clone
     (Self : in out Object; Directory : String := ""; Branch : String := "")
      return Alice.Result.Object'Class
   is abstract;
   --  Clones the repository to a local directory. The implementation should
   --  return the result of the clone operation, which may include the local
   --  path of the cloned repository or an error if the clone operation fails.

   function Get_Remote (Self : in out Object) return String is abstract;
   --  Returns the remote URL of the repository. The implementation should
   --  return the URL of the remote repository associated with this VCS
   --  repository object.

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

end Alice.IFace.VCS.Repository;
