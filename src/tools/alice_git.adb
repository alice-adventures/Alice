-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GitHub_API;

with JSON.Types;
with JSON.Parsers;

with OS_Cmd_Git;

with Simple_Logging;

package body Alice_Git is

   package Log renames Simple_Logging;

   package Types is new JSON.Types (Integer, Float);
   package Parsers is new JSON.Parsers (Types);

   use Types;

   ----------------------
   -- Clone_Repository --
   ----------------------

   function Clone_Repository
     (Repo : String; Directory : String := "") return Boolean
   is
      Success    : Boolean;
      Git_Cmd    : OS_Cmd_Git.Cmd_Type;
      Run_Output : OS_Cmd_Git.Run_Output_Type;
   begin
      if not Git_Cmd.Init then
         return False;
      end if;

      Run_Output := Git_Cmd.Run ("clone -q " & Repo & " " & Directory);
      Success    := (Run_Output.Return_Code = 0);
      Run_Output.Clean;

      return Success;
   end Clone_Repository;

   -----------------------
   -- Create_Repository --
   -----------------------

   function Create_Repository
     (User_Config : User_Config_Type; Repo : String; Description : String)
      return Boolean
   is
   begin
      return
        GitHub_API.Create_A_Repository_For_The_Authenticated_User
          (User_Config, Repo, Description);
   end Create_Repository;

   -------------------------
   -- User_Has_Repository --
   -------------------------

   function User_Has_Repository
     (User_Config : User_Config_Type; Repo : String) return Boolean
   is
   begin
      return User_Has_Repository (User_Config, User_Config.Login, Repo);
   end User_Has_Repository;

   -------------------------
   -- User_Has_Repository --
   -------------------------

   function User_Has_Repository
     (User_Config : User_Config_Type; User : String; Repo : String)
      return Boolean
   is
   begin
      return GitHub_API.Get_A_Repository (User_Config, User, Repo);
   end User_Has_Repository;

end Alice_Git;
