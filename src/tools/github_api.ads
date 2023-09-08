-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

pragma Style_Checks (off);

with Alice_User_Config; use Alice_User_Config;

package GitHub_API is

   package Usr renames Alice_User_Config;

   JSON_File : constant String := ".github.json";

   --  All functions interacting with the GitHub API (all those functions
   --  that have a link to the GitHub doc) work in the same way:
   --
   --     1. Send a request to the GitHub API using curl
   --     2. The response (HTTP code) is saved in a temporary file
   --     3. The JSON object received is saved in the file JSON_File
   --     4. The function returns True if the HTTP code is 200 (Ok)
   --     5. The caller can parse the JSON saved in the JSON_File in the
   --        current directory

   function Create_A_Repository_For_The_Authenticated_User
     (User_Config : User_Config_Type; Repo : String; Description : String)
      return Boolean;
   --  https://docs.github.com/en/rest/repos/repos?apiVersion=2022-11-28#create-a-repository-for-the-authenticated-user
   --
   --  Body parameters:
   --    • name        := Repo
   --    • description := Description

   function Create_A_Repository_Using_A_Template
     (User_Config : User_Config_Type; Template : String; Repo : String;
      Description : String) return Boolean;
   --  https://docs.github.com/en/rest/repos/repos?apiVersion=2022-11-28#create-a-repository-using-a-template
   --
   --  Path parameters:
   --    • template_owner := "alice-adventures"
   --    • template_repo  := Template
   --
   --  Body parameters:
   --    • owner          := User_Config.Login
   --    • name           := Repo
   --    • description    := Description

   function Get_A_Repository
     (User_Config : User_Config_Type; Owner : String; Repo : String)
      return Boolean;
   --  https://docs.github.com/en/rest/repos/repos?apiVersion=2022-11-28#get-a-repository
   --
   --  Path parameters:
   --    • owner := Owner
   --    • repo  := Repo

   function Get_A_User
     (User_Config : User_Config_Type; Name : String) return Boolean;
   --  https://docs.github.com/en/rest/users/users?apiVersion=2022-11-28#get-a-user
   --
   --  Path parameters:
   --    • username := Name

   function Get_The_Authenticated_User
     (User_Config : User_Config_Type) return Boolean;
   --  https://docs.github.com/en/rest/users/users?apiVersion=2022-11-28#get-the-authenticated-user

   function List_Repositories_For_A_User
     (User_Config : User_Config_Type; User : String) return Boolean;
   --  https://docs.github.com/en/rest/repos/repos?apiVersion=2022-11-28#list-repositories-for-a-user
   --
   --  Path parameters:
   --    • username := User
   --
   --  Query Parameters:
   --    • type      := "owner"
   --    • sort      := "full_name"
   --    • direction := "asc"
   --    • per_page  := 100

   function List_Repositories_For_The_Authenticated_User
     (User_Config : User_Config_Type) return Boolean;
   --  https://docs.github.com/en/rest/repos/repos?apiVersion=2022-11-28#list-repositories-for-the-authenticated-user
   --
   --  Query parameters:
   --    • visibility  := "public"
   --    • affiliation := "owner"
   --    • sort        := "full_name"
   --    • direction   := "asc"
   --    • since       := "2023-07-12T00:00:00Z"

end GitHub_API;
