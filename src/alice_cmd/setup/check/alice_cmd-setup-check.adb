-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice_User_Config; use Alice_User_Config;

with OS_Cmd.Alr; use OS_Cmd.Alr;
with OS_Cmd.Curl; use OS_Cmd.Curl;
with OS_Cmd.Git; use OS_Cmd.Git;

with GNAT.OS_Lib;

with Simple_Logging;

use all type GNAT.OS_Lib.String_Access;

package body Alice_Cmd.Setup.Check is

   package Log renames Simple_Logging;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Cmd : in out Cmd_Type; Args : AAA.Strings.Vector)
   is
      OS_Cmd_Alr : Alr_Cmd_Type;
      OS_Cmd_Curl : Curl_Cmd_Type;
      OS_Cmd_Git : Git_Cmd_Type;
      Args_Length : constant Natural := Natural (Args.Length);

   begin

      if Args_Length > 0 then
         Log.Warning ("Too many arguments, ignored");
      end if;

      if OS_Cmd_Alr.Init then
         Log.Info ("alr  command found at '" & OS_Cmd_Alr.Path & "'");
      end if;

      if OS_Cmd_Curl.Init then
         Log.Info ("curl command found at '" & OS_Cmd_Curl.Path & "'");
      end if;

      if OS_Cmd_Git.Init then
         Log.Info ("git  command found at '" & OS_Cmd_Git.Path & "'");
      end if;

      if not Alice_User_Config.Has_User_Config_File then
         return;
      end if;
   end Execute;

end Alice_Cmd.Setup.Check;
