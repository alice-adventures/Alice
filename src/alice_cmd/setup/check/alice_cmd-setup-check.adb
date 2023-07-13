-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;

with Alice_User_Config; use Alice_User_Config;
with Alice_Git;         use Alice_Git;

with OS_Cmd_Alr;
with OS_Cmd_Curl;
with OS_Cmd_Git;

with Simple_Logging;

with Text_IO;

package body Alice_Cmd.Setup.Check is

   package Log renames Simple_Logging;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Cmd : in out Cmd_Type; Args : AAA.Strings.Vector)
   is
      Args_Length : constant Natural := Natural (Args.Length);

      Alr_Cmd  : OS_Cmd_Alr.Cmd_Type;
      Curl_Cmd : OS_Cmd_Curl.Cmd_Type;
      Git_Cmd  : OS_Cmd_Git.Cmd_Type;

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

      --  *REVIEW - Check that GitHub token is truly operative

      if User_Config.Read_From_File then
         if User_Has_Repository (User_Config, "alice-test") then
            Log.Detail ("User has repo 'alice-test'");
         else
            Log.Detail ("Missing repo 'alice-test' for user, creating repo");
            if Create_Repository
                (User_Config, "alice-test",
                 "Test repository used by Alice Adventures")
            then
               Log.Info ("Repository 'alice-test' created");
            else
               Alice_Cmd.Exit_Status := 1;
               Log.Error ("Could no create repository 'alice-test'");
               return;
            end if;
         end if;

         Ada.Directories.Set_Directory ("config");


      end if;

   end Execute;

end Alice_Cmd.Setup.Check;
