-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice_User_Config; use Alice_User_Config;

package Alice_Git is

   GitHub_Root       : constant String := "git@github.com:";
   Alice_Github_Root : constant String := GitHub_Root & "alice-adventures";

   function Clone_Repository
     (Repo : String; Directory : String := "") return Boolean;

   function Create_Repository
     (User_Config : User_Config_Type; Repo : String; Description : String)
      return Boolean;
   --  Create a new repository for the authenticated user.

   function User_Has_Repository
     (User_Config : User_Config_Type; Repo : String) return Boolean;
   --  Return True if user the authenticated user has the repo Repo.

   function User_Has_Repository
     (User_Config : User_Config_Type; User : String; Repo : String)
      return Boolean;
   --  Return True if User user has teh repo Repo.

end Alice_Git;
