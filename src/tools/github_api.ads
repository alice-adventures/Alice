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

   JSON_File : constant String := ".github.json";

   --  All functions interacting with the GitHub API work in the same way:
   --
   --     1. Send a request to the GitHub API using curl
   --     2. The response (HTTP code) is saved in a temporary file
   --     3. The JSON object received is saved in the file JSON_File
   --     4. The function returns True if the HTTP code is 200 (Ok)
   --     5. The caller can parse the JSON saved in the JSON_File

   function Get_A_Repository
     (User_Config : User_Config_Type; Repo : String) return Boolean;
   --  https://docs.github.com/en/rest/repos/repos?apiVersion=2022-11-28#get-a-repository

   function Get_The_Authenticated_User
     (User_Config : User_Config_Type) return Boolean;
   --  https://docs.github.com/en/rest/users/users?apiVersion=2022-11-28#get-the-authenticated-user

end GitHub_API;
