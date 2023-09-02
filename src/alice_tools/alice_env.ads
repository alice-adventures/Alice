-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Alice_Env is

   Remote_Repo      : constant String := "github.com";
   Remote_Repo_Root : constant String := "git@github.com:";

   Alice_Org       : constant String := "alice-adventures";
   Alice_Repo_Root : constant String := "git@github.com:alice-adventures";
   Alice_Index_URL : constant String :=
     "git+https://github.com/alice-adventures/alice-index";

   function Alice_Repository (Name : String) return String is
      (Alice_Org & "/" & Name);
   --  Return the name of the Alice remote repository Name, in the Alice
   --  organization namespace.

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
