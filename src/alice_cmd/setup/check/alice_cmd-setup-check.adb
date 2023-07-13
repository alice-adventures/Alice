-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice_User_Config; use Alice_User_Config;

with GitHub_API; use GitHub_API;

with OS_Cmd_Alr;
with OS_Cmd_Curl;
with OS_Cmd_Git;

with Simple_Logging;

package body Alice_Cmd.Setup.Check is

   package Log renames Simple_Logging;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Cmd : in out Cmd_Type; Args : AAA.Strings.Vector)
   is
      Alr_Cmd     : OS_Cmd_Alr.Cmd_Type;
      Curl_Cmd    : OS_Cmd_Curl.Cmd_Type;
      Git_Cmd     : OS_Cmd_Git.Cmd_Type;
      Args_Length : constant Natural := Natural (Args.Length);
      User_Config : Alice_User_Config.User_Config_Type;
   begin
      if Args_Length > 0 then
         Log.Warning ("Too many arguments, ignored");
      end if;

      if Alr_Cmd.Init then
         Log.Info ("alr  command found at '" & Alr_Cmd.Path & "'");
      end if;

      if Curl_Cmd.Init then
         Log.Info ("curl command found at '" & Curl_Cmd.Path & "'");
      end if;

      if Git_Cmd.Init then
         Log.Info ("git  command found at '" & Git_Cmd.Path & "'");
      end if;

      if not Alice_User_Config.Has_User_Config_File then
         return;
      end if;

      --  *TODO - Check that GitHub token is actually operative
      --  Try to make some GitHub API operation that modifies something
      if User_Config.Read_From_File then
         --  if Create_A_Repository_For_The_Authenticated_User
         --      (User_Config, "alice-test3", "3rd repo test from Ada Github API")
         if Create_A_Repository_Using_A_Template
             (User_Config => User_Config, Template => "project_euler-template",
              Repo        => "alice-test-project_euler",
              Description => "Test created repo from tempoate with API")
         then
            Log.Info ("OK!");
         end if;
      end if;

   end Execute;

end Alice_Cmd.Setup.Check;
