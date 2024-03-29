-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Alice_Environment is

   Environment_Error : exception;

   function Is_Alice_Repository
     (Report_Error : Boolean := True) return Boolean;
   --  Check if the current working directory belongs to the Alice
   --  repository.

   function Is_Alice_Root_Dir (Report_Error : Boolean := True) return Boolean;
   --  Check if the current working directory is a clone of the Alice
   --  repository and it is the root directory.

   function Get_Alice_Root_Dir return String;
   --  Return the root directory of Alice Adventures. When called before
   --  Is_Alice_Root_Dir confirmation it returns an empty String.

end Alice_Environment;
