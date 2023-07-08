-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GNAT.AWK;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;

use all type GNAT.OS_Lib.String_Access;

with JSON.Types;
with JSON.Parsers;

with OS_Cmd.Curl; use OS_Cmd.Curl;
with OS_Cmd.Git;  use OS_Cmd.Git;

with Simple_Logging;

with Text_IO;

with TOML;
with TOML.File_IO;

package body Alice_User_Config is

   package Log renames Simple_Logging;

   Key_Email : constant String := "email";
   Key_Login : constant String := "login";
   Key_Name  : constant String := "name";
   Key_Token : constant String := "token";

   ------------
   -- Author --
   ------------

   function Author (User_Config : User_Config_Type) return Unbounded_String is
     (User_Config.User_Name);

   -----------
   -- Email --
   -----------

   function Email (User_Config : User_Config_Type) return Unbounded_String is
     (User_Config.User_Email);

   -----------
   -- Login --
   -----------

   function Login (User_Config : User_Config_Type) return Unbounded_String is
     (User_Config.GitHub_Login);

   -----------
   -- Token --
   -----------

   function Token (User_Config : User_Config_Type) return Unbounded_String is
     (User_Config.GitHub_Token);

   -----------
   -- Token --
   -----------

   function Get_Info_From_Token
     (User_Config : in out User_Config_Type; Token : Unbounded_String)
      return Boolean
   is
      Success : Boolean := False;
   begin
      User_Config.GitHub_Token := Token;

      Success := User_Config.Get_Info_From_GitHub_Token;
      if Success then
         Success := User_Config.Get_Info_From_Git_Config;
      end if;

      return Success;
   end Get_Info_From_Token;

   ---------------------
   -- Has_Config_File --
   ---------------------

   function Has_Config_File (Filename : String) return Boolean is
      Path    : GNAT.OS_Lib.String_Access;
      Success : Boolean;
   begin
      Path    := GNAT.OS_Lib.Locate_Regular_File (Filename, Config_Directory);
      Success := (Path /= null);

      if Success then
         Log.Debug ("config file '" & Filename & "' found at " & Path.all);
         GNAT.OS_Lib.Free (Path);
      end if;

      return Success;
   end Has_Config_File;

   --------------------------
   -- Has_User_Config_File --
   --------------------------

   function Has_User_Config_File return Boolean is
      Success : Boolean := Has_Config_File (User_Config_File);
   begin
      if not Success then
         Log.Error
           ("User configuration file not found, please run 'alice config'");
      end if;

      return Success;
   end Has_User_Config_File;

   ----------------------------
   -- Check_User_Config_File --
   ----------------------------

   function Check_User_Config_File return Boolean is
      Success          : Boolean := False;
      Config_From_File : User_Config_Type;
   begin
      if not Has_User_Config_File then
         return False;
      end if;

      Config_From_File := Read_From_File;
      if Success then
         declare
            Config_From_Token : User_Config_Type;
         begin
            Success :=
              Config_From_Token.Get_Info_From_Token (Config_From_File.Token);
            if not Success or else Config_From_File /= Config_From_File then
               Log.Warning
                 ("User configuration file outdated, please update it");
            end if;
         end;
      end if;

      return Success;
   end Check_User_Config_File;

   --------------------
   -- Read_From_File --
   --------------------

   function Read_From_File return User_Config_Type is
      User_Config : User_Config_Type;
      Read_Result : TOML.Read_Result;
   begin
      if not Has_User_Config_File then
         return User_Config;
      end if;

      Read_Result :=
        TOML.File_IO.Load_File
          (Config_Directory & GNAT.Directory_Operations.Dir_Separator &
           User_Config_File);

      if Read_Result.Success then
         if Read_Result.Value.Has (Key_Login) then
            User_Config.GitHub_Login :=
              To_Unbounded_String
                (Read_Result.Value.Get (Key_Login).As_String);
         else
            Log.Error
              ("Could not get " & Key_Login & " from user configuration file");
         end if;
         if Read_Result.Value.Has (Key_Token) then
            User_Config.GitHub_Token :=
              To_Unbounded_String
                (Read_Result.Value.Get (Key_Token).As_String);
         else
            Log.Error
              ("Could not get " & Key_Token & " from user configuration file");
         end if;
         if Read_Result.Value.Has (Key_Name) then
            User_Config.User_Name :=
              To_Unbounded_String (Read_Result.Value.Get (Key_Name).As_String);
         end if;
         if Read_Result.Value.Has (Key_Email) then
            User_Config.User_Name :=
              To_Unbounded_String
                (Read_Result.Value.Get (Key_Email).As_String);
         end if;
      else
         Log.Error ("Could not load user configuration file");
         Log.Error (To_String (Read_Result.Message));
      end if;

      return User_Config;
   end Read_From_File;

   -------------------
   -- Write_To_File --
   -------------------

   function Write_To_File (User_Config : User_Config_Type) return Boolean is
      Success     : Boolean                  := True;
      Table       : constant TOML.TOML_Value := TOML.Create_Table;
      Config_File : Text_IO.File_Type;
   begin
      Table.Set
        (Key_Login, TOML.Create_String (To_String (User_Config.GitHub_Login)));
      Table.Set
        (Key_Name, TOML.Create_String (To_String (User_Config.User_Name)));
      Table.Set
        (Key_Email, TOML.Create_String (To_String (User_Config.User_Email)));
      Table.Set
        (Key_Token, TOML.Create_String (To_String (User_Config.GitHub_Token)));

      Config_File.Create
        (Text_IO.Out_File,
         Config_Directory & GNAT.Directory_Operations.Dir_Separator &
         User_Config_File);

      TOML.File_IO.Dump_To_File (Table, Config_File);

      Config_File.Close;
      return Success;
   end Write_To_File;

   --------------------------------
   -- Get_Info_From_GitHub_Token --
   --------------------------------

   function Get_Info_From_GitHub_Token
     (User_Config : in out User_Config_Type) return Boolean
   is
      Success     : Boolean := False;
      OS_Cmd_Curl : OS_Cmd_Curl_Type;
      Run_Output  : OS_Cmd.Run_Output_Type;
   begin
      if User_Config.Token = To_Unbounded_String ("") then
         Log.Error ("GitHub token not set");
         return False;
      end if;

      OS_Cmd_Curl.Init;
      Run_Output :=
        OS_Cmd_Curl.Run
          ("-s -L -H Authorization:\ Bearer\ " &
           To_String (User_Config.Token) & " " &
           "https://api.github.com/user");

      declare
         package Types is new JSON.Types (Integer, Float);
         package Parsers is new JSON.Parsers (Types);

         use Types;

         Parser : Parsers.Parser :=
           Parsers.Create_From_File (Run_Output.Temp_File.all);

         Object : constant JSON_Value := Parser.Parse;

         function Value_Str (Object : JSON_Value) return String renames
           Types.Value;
      begin
         if Object.Contains (Key_Login) then
            Log.Debug ("GitHub type : " & Image (Object.Get ("type")));

            if Object.Get ("type").Kind = String_Kind
              and then Value_Str (Object.Get ("type")) = "User"
            then
               Log.Debug ("GitHub login: " & Image (Object.Get (Key_Login)));
               Log.Debug ("GitHub name : " & Image (Object.Get (Key_Name)));
               Log.Debug ("GitHub email: " & Image (Object.Get (Key_Email)));

               if Object.Get (Key_Login).Kind = String_Kind then
                  User_Config.GitHub_Login :=
                    To_Unbounded_String (Value_Str (Object.Get (Key_Login)));
                  Success                  := True;
               else
                  Log.Error ("Could not get login from GitHub token");
               end if;

               if Object.Get (Key_Name).Kind = String_Kind then
                  User_Config.User_Name :=
                    To_Unbounded_String (Value_Str (Object.Get (Key_Name)));
               end if;

               if Object.Get (Key_Email).Kind = String_Kind then
                  User_Config.User_Email :=
                    To_Unbounded_String (Value_Str (Object.Get (Key_Email)));
               end if;
            end if;
         else
            Log.Error ("Token is not associated to a valid GitHub account");
         end if;
      end;

      OS_Cmd_Curl.Clean (Run_Output);

      return Success;
   end Get_Info_From_GitHub_Token;

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
      Run_Output := OS_Cmd_Git.Run ("config -l");

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

end Alice_User_Config;
