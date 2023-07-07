-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with CLIC.User_Input;
with CLIC.Config;

with TOML;

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

      --  declare
      --     function Foo (User_Input : String) return CLIC.User_Input.Answer_Kind
      --     is
      --     begin
      --        return CLIC.User_Input.Yes;
      --     end Foo;

      --     function Bar (User_Input : String) return Boolean is
      --     begin
      --        return True;
      --     end Bar;
      --  begin
      --     declare
      --        Answer : constant CLIC.User_Input.Answer_With_Input :=
      --          CLIC.User_Input.Validated_Input
      --            ("Enter your name", "(Default '" & To_String (Author) & "'): ",
      --             [True, True, False], Foo'Access, Is_Valid => Bar'Access);
      --     begin
      --        if Answer.Length > 0 then
      --           Author := To_Unbounded_String (AAA.Strings.Trim (Answer.Input));
      --        end if;
      --     end;

      --     Text_IO.Put_Line ("Author=""" & To_String (Author) & """");
      --     Text_IO.New_Line;

      --     declare
      --        Answer : constant CLIC.User_Input.Answer_With_Input :=
      --          CLIC.User_Input.Validated_Input
      --            ("Enter your Github username",
      --             "(Default '" & To_String (User_Name) & "'): ",
      --             [True, True, False], Foo'Access, Is_Valid => Bar'Access);
      --     begin
      --        User_Name := To_Unbounded_String (AAA.Strings.Trim (Answer.Input));
      --     end;

      --     Text_IO.Put_Line ("User_Name=""" & To_String (User_Name) & """");
      --     Text_IO.New_Line;

      --     User_Email :=
      --       To_Unbounded_String
      --         (CLIC.User_Input.Query_String
      --            ("Enter your email> ", To_String (User_Email),
      --             Bar'Unrestricted_Access));
      --     Text_IO.Put_Line ("User_Name=""" & To_String (User_Email) & """");
      --  end;

      declare
         T : TOML.TOML_Value := TOML.Create_Table;
      begin
         T.Set ("username", TOML.Create_String ("Francesc Rocher"));
         T.Set ("github_login", TOML.Create_String ("rocher"));
         T.Set
           ("github_token",
            TOML.Create_String ("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"));
         Text_IO.Put_Line (T.Dump_As_String);
      end;

   end Execute;

end Alice_Cmd.Setup.Config;
