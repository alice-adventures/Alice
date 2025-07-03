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

   type Object is interface;

   function Clone (Self : in out Object) return Alice.Result.Object'Class
   is abstract;
   --  Clones the repository to a local directory. The implementation should
   --  return the result of the clone operation, which may include the local
   --  path of the cloned repository or an error if the clone operation fails.

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
