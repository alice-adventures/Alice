-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Text_IO;

with OS_Cmd.Curl; use OS_Cmd.Curl;
with OS_Cmd.Git;  use OS_Cmd.Git;

with GNAT.AWK;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Regpat;

with JSON.Types;
with JSON.Parsers;

with Simple_Logging;

with TOML;
with TOML.File_IO;

package body Alice_User_Config is

   package Log renames Simple_Logging;

   ------------
   -- Author --
   ------------

   function Author (User_Config : User_Config_Type) return Unbounded_String is
     (User_Config.User_Name);

   -----------
   -- Login --
   -----------

   function Login (User_Config : User_Config_Type) return Unbounded_String is
     (User_Config.Github_Login);

   -----------
   -- Email --
   -----------

   function Email (User_Config : User_Config_Type) return Unbounded_String is
     (User_Config.User_Email);

   -----------
   -- Token --
   -----------

   function Token (User_Config : User_Config_Type) return Unbounded_String is
     (User_Config.Github_Token);

   -----------
   -- Token --
   -----------

   procedure Token
     (User_Config : in out User_Config_Type; Token : Unbounded_String)
   is
   begin
      User_Config.Github_Token := Token;
   end Token;

   --------------------------------
   -- Get_Info_From_Github_Token --
   --------------------------------

   function Get_Info_From_Github
     (User_Config : in out User_Config_Type) return Boolean
   is
      Success     : Boolean := False;
      OS_Cmd_Curl : OS_Cmd_Curl_Type;
      Run_Output  : OS_Cmd.Run_Output_Type;
   begin
      if User_Config.Token = To_Unbounded_String ("") then
         Log.Error ("Github token not set");
         return False;
      end if;

      OS_Cmd_Curl.Init;
      OS_Cmd_Curl.Run
        ("-s -L -H Authorization:\ Bearer\ " & To_String (User_Config.Token) &
         " " & "https://api.github.com/user",
         Run_Output);

      declare
         package Types is new JSON.Types (Integer, Float);
         package Parsers is new JSON.Parsers (Types);

         use Types;

         Parser : Parsers.Parser :=
           Parsers.Create_From_File (Run_Output.Temp_File.all);

         Object : JSON_Value := Parser.Parse;

         function Value_Str (Object : JSON_Value) return String renames
           Types.Value;
      begin
         if Object.Contains ("login") then
            Log.Debug ("Github type : " & Image (Object.Get ("type")));

            if Object.Get ("type").Kind = String_Kind
              and then Value_Str (Object.Get ("type")) = "User"
            then
               Log.Debug ("Github login: " & Image (Object.Get ("login")));
               Log.Debug ("Github name : " & Image (Object.Get ("name")));
               Log.Debug ("Github email: " & Image (Object.Get ("email")));

               if Object.Get ("login").Kind = String_Kind then
                  User_Config.Github_Login :=
                    To_Unbounded_String (Value_Str (Object.Get ("login")));
                  Success                  := True;
               else
                  Log.Error ("Could not get login from Github token");
               end if;

               if Object.Get ("name").Kind = String_Kind then
                  User_Config.User_Name :=
                    To_Unbounded_String (Value_Str (Object.Get ("name")));
               end if;

               if Object.Get ("email").Kind = String_Kind then
                  User_Config.User_Email :=
                    To_Unbounded_String (Value_Str (Object.Get ("email")));
               end if;
            end if;
         else
            Log.Error ("Token is not associated to a valid Github account");
         end if;
      end;

      OS_Cmd_Curl.Clean (Run_Output);

      return Success;
   end Get_Info_From_Github;

   ------------------------------
   -- Get_Info_From_Git_Config --
   ------------------------------

   function Get_Info_From_Git_Config
     (User_Config : in out User_Config_Type) return Boolean
   is
      OS_Cmd_Git  : OS_Cmd_Git_Type;
      Run_Output  : OS_Cmd.Run_Output_Type;
      Match_Count : Natural := 0;

      function Field_Str (RanK : GNAT.AWK.Count) return String renames
        GNAT.AWK.Field;

      procedure User_Name_Match is
      begin
         Log.Debug ("User_Name_Match");
         Match_Count := @ + 1;
         if User_Config.User_Name = To_Unbounded_String ("") then
            User_Config.User_Name := To_Unbounded_String (Field_Str (2));
         end if;
      end User_Name_Match;

      procedure User_Email_Match is
      begin
         Match_Count := @ + 1;
         if User_Config.User_Email = To_Unbounded_String ("") then
            User_Config.User_Email := To_Unbounded_String (Field_Str (2));
         end if;
      end User_Email_Match;

   begin
      OS_Cmd_Git.Init;
      OS_Cmd_Git.Run ("config -l", Run_Output);

      GNAT.AWK.Add_File (Run_Output.Temp_File.all);
      GNAT.AWK.Set_Field_Separators ("=");
      GNAT.AWK.Register (1, "user.name", User_Name_Match'Unrestricted_Access);
      GNAT.AWK.Register
        (1, "user.email", User_Email_Match'Unrestricted_Access);

      GNAT.AWK.Parse;
      GNAT.AWK.Close (GNAT.AWK.Default_Session.all);

      OS_Cmd_Git.Clean (Run_Output);

      return (Match_Count = 2);
   end Get_Info_From_Git_Config;

   --------------------------
   -- Has_User_Config_File --
   --------------------------

   function Has_User_Config_File return Boolean is (True);

   ----------------------------
   -- Check_User_Config_File --
   ----------------------------

   function Check_User_Config_File return Boolean is (True);

   ---------------------------
   -- Read_User_Config_File --
   ---------------------------

   function Read_User_Config_File return User_Config_Type is
      User_Config : User_Config_Type;
      Read_Result : TOML.Read_Result;
   begin
      Read_Result :=
        TOML.File_IO.Load_File
          (Config_Directory & GNAT.Directory_Operations.Dir_Separator &
           User_Config_File);

      return User_Config;
   end Read_User_Config_File;

   ----------------------------
   -- Write_User_Config_File --
   ----------------------------

   function Write_User_Config_File
     (User_Config : User_Config_Type) return Boolean
   is
      Success     : Boolean         := False;
      Table       : TOML.TOML_Value := TOML.Create_Table;
      Config_File : Text_IO.File_Type;
   begin
      Table.Set
        ("login", TOML.Create_String (To_String (User_Config.Github_Login)));
      Table.Set
        ("name", TOML.Create_String (To_String (User_Config.User_Name)));
      Table.Set
        ("email", TOML.Create_String (To_String (User_Config.User_Email)));
      Table.Set
        ("token", TOML.Create_String (To_String (User_Config.Github_Token)));

      Config_File.Create
        (Text_IO.Out_File,
         Config_Directory & GNAT.Directory_Operations.Dir_Separator &
         User_Config_File);

      TOML.File_IO.Dump_To_File (Table, Config_File);

      Config_File.Close;
      return Success;
   end Write_User_Config_File;

end Alice_User_Config;
