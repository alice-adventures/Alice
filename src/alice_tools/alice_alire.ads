-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Alice_Alire is

   Alice_Index_URL : constant String :=
     "git+https://github.com/alice-adventures/alice-index";

   procedure Ensure_Alice_Index;
   --  Add the Alice index in the current Alire configuration, if not exists.
   --  Return True if the Alice index already exists or has been successfully
   --  added.

   procedure Update_Indexes;
   --  Update all Alire indexes.

   procedure Build_Crate (Args : String := "");
   --  Build the crate in the current directory with the given arguments.

end Alice_Alire;
