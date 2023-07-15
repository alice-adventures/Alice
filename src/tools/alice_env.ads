-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Alice_Env is

   function Is_Alice_Repository
     (Report_Error : Boolean := True) return Boolean;
   --  Check if the current working directory belongs to the Alice
   --  repository.

   function Is_Alice_Root_Dir (Report_Error : Boolean := True) return Boolean;
   --  Check if the current working directory belongs to the Alice repository
   --  and it is the root directory.

   function Get_Alice_Root_Dir return String;
   --  Return to root directory of Alice Adventures. Must be called after
   --  Is_Alice_Root_Dir confirmation.

end Alice_Env;
