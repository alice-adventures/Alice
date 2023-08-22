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

   function Clone_GitHub_Repository
     (Repository : String; Directory : String := "") return Boolean;
   --  Clone the given Repository in the directory Directory.

   function Create_GitHub_Repository
     (User_Config : User_Config_Type; Repository : String;
      Description : String) return Boolean;
   --  Create the repository "<User_Config.Author>/Repository" for the
   --  authenticated user.

   function User_Has_GitHub_Repository
     (User_Config : User_Config_Type; Repository : String) return Boolean;
   --  Return True if the repository "<User_Config.Author>/Repository" exists
   --  in GitHub.

   function Exists_GitHub_Repository
     (User_Config : User_Config_Type; User : String; Repository : String)
      return Boolean;
   --  Return True if the repository "User/Repository" exists in GitHub.

   function Is_Git_Clone_Of (Server, Repository : String) return Boolean;
   --  Return True if the current working directory is a clone of the
   --  repository with the origin "Server:Repository". Internally, a regular
   --  expression is created to match ssh and http transports. For example,
   --  for Server="server.com" and Repository="user/repo", the regular
   --  expression match "git@server.com:user/Repository.git" (ssh) and
   --  "https://server.com/user/Repository.git" (https).

end Alice_Git;
