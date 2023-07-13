-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

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
      return
        User_Has_Repository (User_Config, To_String (User_Config.Login), Repo);
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
