-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice_Env;
with Alice_User_Config; use Alice_User_Config;

package Alice_Git is

   package Env renames Alice_Env;
   package Usr renames Alice_User_Config;

   function Clone_Remote_Repository
     (Repository : String; Directory : String := "") return Boolean;
   --  Clone the remote Repository in the directory Directory.

   function Create_Remote_Repository
     (User_Config : User_Config_Type; Repository : String;
      Description : String) return Boolean;
   --  Create the remote repository "<User_Config.Author>/Repository" for the
   --  authenticated user.

   function User_Has_Remote_Repository
     (User_Config : User_Config_Type; Repository : String) return Boolean;
   --  Return True if the remote repository "<User_Config.Author>/Repository"
   --  exists.

   function Exists_Remote_Repository
     (User_Config : Usr.User_Config_Type; User : String; Repository : String)
      return Boolean;
   --  Return True if the remote repository "User/Repository" exists.

   function Is_Clone_Of (Repository : String) return Boolean;
   --  Return True if the current working directory is a clone of the
   --  Repository.
   --
   --  Internally, a regular expression is created to match git and https
   --  transport protocols. For example, for Host="github.com" and
   --  Repository="bob/foo", the regular expression match
   --  "git@github.com:bob/foo.git" (git) and
   --  "https://github.com/bob/foo.git" (https).

end Alice_Git;
