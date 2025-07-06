-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Alice.Env is

   Environment_Error : exception;

   function Is_Alice_Repository
     (Report_Error : Boolean := True) return Boolean;
   --  Check if the current working directory belongs to the Alice repository.
   --  This function checks if the current directory is a clone of the Alice
   --  repository by looking for a specific file or directory that indicates
   --  the presence of the Alice repository. If Report_Error is True, it
   --  raises Environment_Error if the check fails.

   function Is_Alice_Root_Dir (Report_Error : Boolean := True) return Boolean;
   --  Check if the current working directory is a clone of the Alice
   --  repository and it is the root directory. This function verifies that
   --  the current directory is the root of the Alice repository by checking
   --  for the presence of specific files or directories that are only found
   --  at the root of the repository. If Report_Error is True, it raises
   --  Environment_Error if the check fails. This function should be called
   --  before calling Get_Alice_Root_Dir to ensure that the current directory
   --  is indeed the root of the Alice repository.

   function Get_Alice_Root_Dir return String;
   --  Return the root directory of Alice Adventures. This function returns
   --  the absolute path to the root directory of the Alice repository. If
   --  called before confirming that the current directory is the root of the
   --  Alice repository using Is_Alice_Root_Dir, it returns an empty String.
   --  This function is useful for obtaining the base directory of the Alice
   --  application, which can be used for various purposes such as loading
   --  configuration files, accessing resources, or performing operations that
   --  require knowledge of the repository structure.

end Alice.Env;
