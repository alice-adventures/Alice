-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with OS_Cmd.Curl; use OS_Cmd.Curl;
with OS_Cmd.Git;  use OS_Cmd.Git;

with GNAT.AWK;
with GNAT.OS_Lib;
with GNAT.Regpat;

with Simple_Logging;
with Text_IO;

use all type GNAT.OS_Lib.String_Access;

package body Alice_Cmd.Setup.Config is

   package Log renames Simple_Logging;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Cmd : in out Cmd_Type; Args : AAA.Strings.Vector)
   is
      OS_Cmd_Curl : OS_Cmd_Curl_Type;
      OS_Cmd_Git  : OS_Cmd_Git_Type;
      Run_Output  : OS_Cmd.Run_Output_Type;
      Args_Length : constant Natural := Natural (Args.Length);

      Author       : Unbounded_String := To_Unbounded_String ("");
      User_Name    : Unbounded_String := To_Unbounded_String ("");
      User_Email   : Unbounded_String := To_Unbounded_String ("");
      Github_Token : Unbounded_String := To_Unbounded_String ("");
   begin

      if Args_Length > 0 then
         Log.Warning ("Too many arguments, ignored");
      end if;

      OS_Cmd_Curl.Init;
      OS_Cmd_Git.Init;

      OS_Cmd_Git.Run ("config -l", Run_Output);

      declare
         function Field_Str (Rank : GNAT.AWK.Count) return String renames
           GNAT.AWK.Field;

         procedure User_Name_Match is
         begin
            Author := To_Unbounded_String (Field_Str (2));
         end User_Name_Match;

         procedure User_Email_Match is
         begin
            User_Email := To_Unbounded_String (Field_Str (2));
         end User_Email_Match;
      begin
         GNAT.AWK.Add_File (Run_Output.Temp_File.all);
         GNAT.AWK.Set_Field_Separators ("=");
         GNAT.AWK.Register
           (1, "user.name", User_Name_Match'Unrestricted_Access);
         GNAT.AWK.Register
           (1, "user.email", User_Email_Match'Unrestricted_Access);

         GNAT.AWK.Parse;
         GNAT.AWK.Close (GNAT.AWK.Default_Session.all);
      end;

      Text_IO.Put_Line ("Author:   " & To_String (Author));
      Text_IO.Put_Line ("Username: " & To_String (User_Name));
      Text_IO.Put_Line ("email:    " & To_String (User_Email));

      OS_Cmd_Git.Clean (Run_Output);
   end Execute;

end Alice_Cmd.Setup.Config;
