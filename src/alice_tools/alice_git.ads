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

   function Is_Clone_Of
     (Repository : String; Server : String := Env.Remote_Repo) return Boolean;
   --  Return True if the current working directory is a clone of the
   --  repository with the origin "Server:Repository".
   --
   --  Internally, a regular expression is created to match ssh and http
   --  transports. For example, for Server="server.com" and
   --  Repository="user/repo", the regular expression match
   --  "git@server.com:user/Repository.git" (ssh) and
   --  "https://server.com/user/Repository.git" (https).

end Alice_Git;
