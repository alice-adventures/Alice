-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice_User_Config; use Alice_User_Config;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with OS_Cmd.Alr;  use OS_Cmd.Alr;
with OS_Cmd.Curl; use OS_Cmd.Curl;
with OS_Cmd.Git;  use OS_Cmd.Git;

with GNAT.AWK;
with GNAT.OS_Lib;
with GNAT.Regpat;

with Simple_Logging;
with Text_IO;

use all type GNAT.OS_Lib.String_Access;

package body Alice_Cmd.Setup.Check is

   package Log renames Simple_Logging;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Cmd : in out Cmd_Type; Args : AAA.Strings.Vector)
   is
      OS_Cmd_Alr  : OS_Cmd_Alr_Type;
      OS_Cmd_Curl : OS_Cmd_Curl_Type;
      OS_Cmd_Git  : OS_Cmd_Git_Type;
      Run_Output  : OS_Cmd.Run_Output_Type;
      Args_Length : constant Natural := Natural (Args.Length);

   begin

      if Args_Length > 0 then
         Log.Warning ("Too many arguments, ignored");
      end if;

      OS_Cmd_Alr.Init;
      Text_IO.Put_Line ("found alr  at '" & OS_Cmd_Alr.Path & "'");

      OS_Cmd_Curl.Init;
      Text_IO.Put_Line ("found curl at '" & OS_Cmd_Curl.Path & "'");

      OS_Cmd_Git.Init;
      Text_IO.Put_Line ("found git  at '" & OS_Cmd_Git.Path & "'");

      OS_Cmd_Git.Run ("remote -v", Run_Output);

      declare
         Alice_Repo_Matcher : constant GNAT.Regpat.Pattern_Matcher :=
           GNAT.Regpat.Compile ("^.*github\.com.alice-adventures.Alice\.git$");

         Matches : Natural := 0;

         procedure Field_Match is
         begin
            Matches := @ + 1;
         end Field_Match;
      begin
         GNAT.AWK.Add_File (Run_Output.Temp_File.all);
         GNAT.AWK.Register (1, "origin", Field_Match'Unrestricted_Access);
         GNAT.AWK.Register
           (2, Alice_Repo_Matcher, Field_Match'Unrestricted_Access);

         GNAT.AWK.Parse;
         GNAT.AWK.Close (GNAT.AWK.Default_Session.all);

         if Matches = 4 then
            Text_IO.Put_Line ("alice repository detected");
         else
            Log.Error
              ("command 'alice' must be invoked inside the Alice repository");
         end if;
      end;

      OS_Cmd_Git.Clean (Run_Output);

      declare
         Success     : Boolean          := False;
         User_Config : User_Config_Type;
         My_Token    : Unbounded_String :=
           To_Unbounded_String ("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");
      begin
         User_Config.Token (My_Token);
         Success := User_Config.Get_Info_From_Github;
         if Success then
            Text_IO.Put_Line (User_Config'Image);
         else
            Text_IO.Put_Line ("Could not get info from github");
         end if;

         Success := User_Config.Get_Info_From_Git_Config;
         if Success then
            Text_IO.Put_Line (User_Config'Image);
         else
            Text_IO.Put_Line ("Could not get info from git");
         end if;

         Success := Alice_User_Config.Write_User_Config_File (User_Config);
      end;

   end Execute;

end Alice_Cmd.Setup.Check;
