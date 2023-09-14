-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Text_IO;
with GNAT.AWK;

with JSON.Types;
with JSON.Parsers;
with Simple_Logging;
with SPDX.Licenses;
with TOML;
with TOML.File_IO;

with Alice_Configuration;
with GitHub.API;
with OS_Cmd_Git;

package body GitHub.Profile is

   package Conf renames Alice_Configuration;
   package Log renames Simple_Logging;

   ------------
   -- Author --
   ------------

   function Name (Profile : Profile_Type) return String is
     (To_String (Profile.User_Name));

   -----------
   -- Email --
   -----------

   function Email (Profile : Profile_Type) return String is
     (To_String (Profile.User_Email));

   -----------
   -- Login --
   -----------

   function Login (Profile : Profile_Type) return String is
     (To_String (Profile.GitHub_Login));

   -----------
   -- Token --
   -----------

   function Token (Profile : Profile_Type) return String is
     (To_String (Profile.GitHub_Token));

   ----------
   -- SPDX --
   ----------

   function SPDX (Profile : Profile_Type) return String is
     (To_String (Profile.SPDX_Id));

   ----------------------------
   -- Load_From_GitHub_Token --
   ----------------------------

   procedure Load_From_GitHub_Token
     (Profile : in out Profile_Type; Token : String)
   is
   begin
      Profile.GitHub_Token := To_Unbounded_String (Token);
      GitHub.API.Get_The_Authenticated_User (Profile);

      declare
         package Types is new JSON.Types (Integer, Float);
         package Parsers is new JSON.Parsers (Types);

         use Types;

         Parser : Parsers.Parser      :=
           Parsers.Create_From_File (GitHub.API.JSON_File);
         Object : constant JSON_Value := Parser.Parse;

         function Value_Str (Object : JSON_Value) return String renames
           Types.Value;
      begin
         if not Object.Contains (Key_Login) then
            raise Error.Load_Profile with "Invalid token, login not found";
         end if;

         if Object.Get (Key_Login).Kind /= String_Kind then
            raise Error.Load_Profile with "Invalid login found";
         end if;

         Log.Debug ("JSON.type : " & Image (Object.Get ("type")));
         Log.Debug ("JSON.login: " & Image (Object.Get (Key_Login)));
         Log.Debug ("JSON.name : " & Image (Object.Get (Key_Name)));
         Log.Debug ("JSON.email: " & Image (Object.Get (Key_Email)));

         Profile.GitHub_Login :=
           To_Unbounded_String (Value_Str (Object.Get (Key_Login)));
         Profile.Valid_Token  := True;

         if Object.Get (Key_Name).Kind = String_Kind then
            Profile.User_Name :=
              To_Unbounded_String (Value_Str (Object.Get (Key_Name)));
         end if;

         if Object.Get (Key_Email).Kind = String_Kind then
            Profile.User_Email :=
              To_Unbounded_String (Value_Str (Object.Get (Key_Email)));
         end if;
      end;
   end Load_From_GitHub_Token;

   --------------------------
   -- Load_From_Git_Config --
   --------------------------

   procedure Load_From_Git_Config (Profile : in out Profile_Type) is
      Cmd_Git     : OS_Cmd_Git.Cmd_Type;
      Run_Output  : OS_Cmd_Git.Run_Output_Type;
      Match_Count : Natural := 0;

      function Field_Str (RanK : GNAT.AWK.Count) return String renames
        GNAT.AWK.Field;

      procedure User_Name_Match is
      begin
         Log.Debug ("AWK user.name match");
         Match_Count := @ + 1;
         if Profile.User_Name = Null_Unbounded_String then
            Profile.User_Name := To_Unbounded_String (Field_Str (2));
            Log.Debug ("  * set User_Name = " & Field_Str (2));
         else
            Log.Debug ("  * User_Name already set, skip value");
         end if;
      end User_Name_Match;

      procedure User_Email_Match is
      begin
         Log.Debug ("AWK user.email match");
         Match_Count := @ + 1;
         if Profile.Email = Null_Unbounded_String then
            Profile.User_Email := To_Unbounded_String (Field_Str (2));
            Log.Debug ("  * set User_Email = " & Field_Str (2));
         else
            Log.Debug ("  * User_Email already set, skip value");
         end if;
      end User_Email_Match;

   begin
      Cmd_Git.Init;

      Log.Detail ("Retrieving information from 'git config'");

      Run_Output := Cmd_Git.Run ("config -l");

      GNAT.AWK.Add_File (Run_Output.Temp_File.all);
      GNAT.AWK.Set_Field_Separators ("=");
      GNAT.AWK.Register (1, "user.name", User_Name_Match'Unrestricted_Access);
      GNAT.AWK.Register
        (1, "user.email", User_Email_Match'Unrestricted_Access);

      GNAT.AWK.Parse;
      GNAT.AWK.Close (GNAT.AWK.Default_Session.all);

      Run_Output.Clean;

      if Match_Count /= 2 then
         raise GitHub_Profile_Error
           with "Could not set name or email from git configuration";
      end if;
   end Load_From_Git_Config;

   --------------------
   -- Load_From_File --
   --------------------

   procedure Load_From_File (Profile : in out Profile_Type) is
      use Ada.Directories;
      Read_Result : TOML.Read_Result;
   begin
      Read_Result :=
        TOML.File_IO.Load_File
          (Compose
             (Containing_Directory => Conf.Local.Config_Directory,
              Name                 => Conf.Local.Profile));
      Log.Debug ("TOML.File_IO.Load_File =" & Read_Result'Image);

      if not Read_Result.Success then
         Log.Error (To_String (Read_Result.Message));
         raise GitHub_Profile_Error
           with "Could not load user configuration file";
      end if;

      if Read_Result.Value.Has (Key_Login) then
         Profile.GitHub_Login :=
           To_Unbounded_String (Read_Result.Value.Get (Key_Login).As_String);
         Log.Debug ("Read_Result (Login) =" & Profile.Login);
      else
         raise GitHub_Profile_Error
           with "Could not get " & Key_Login & " from profile file";
      end if;
      if Read_Result.Value.Has (Key_Token) then
         Profile.GitHub_Token :=
           To_Unbounded_String (Read_Result.Value.Get (Key_Token).As_String);
         Profile.Valid_Token  := True;
         Log.Debug ("Read_Result (Token) : exists (valid)");
      else
         raise GitHub_Profile_Error
           with "Could not get " & Key_Token & " from profile file";
      end if;
      if Read_Result.Value.Has (Key_Name) then
         Profile.User_Name :=
           To_Unbounded_String (Read_Result.Value.Get (Key_Name).As_String);
         Log.Debug ("Read_Result (Name) =" & Profile.Name);
      end if;
      if Read_Result.Value.Has (Key_Email) then
         Profile.User_Email :=
           To_Unbounded_String (Read_Result.Value.Get (Key_Email).As_String);
         Log.Debug ("Read_Result (Email) =" & Profile.Email);
      end if;
      if Read_Result.Value.Has (Key_SPDX_Id) then
         Profile.SPDX_Id :=
           To_Unbounded_String (Read_Result.Value.Get (Key_SPDX_Id).As_String);
         Log.Debug ("Read_Result (SPDX_Id) =" & Profile.SPDX);
      end if;

      Log.Debug ("Profile.Load_Profile =" & Profile'Image);
   end Load_From_File;

   ------------------
   -- Save_To_File --
   ------------------

   procedure Save_To_File (Profile : Profile_Type) is
      use Ada.Directories;
      Table      : constant TOML.TOML_Value := TOML.Create_Table;
      Profile_FD : Ada.Text_IO.File_Type;
   begin
      Table.Set (Key_Login, TOML.Create_String (Profile.Login));
      Table.Set (Key_Name, TOML.Create_String (Profile.Name));
      Table.Set (Key_Email, TOML.Create_String (Profile.Email));
      Table.Set (Key_Token, TOML.Create_String (Profile.Token));
      Table.Set (Key_SPDX_Id, TOML.Create_String (Profile.SPDX));

      Profile_FD.Create
        (Ada.Text_IO.Out_File,
         Compose
           (Containing_Directory => Conf.Local.Config_Directory,
            Name                 => Conf.Local.Profile));

      TOML.File_IO.Dump_To_File (Table, Profile_FD);

      Profile_FD.Close;
      Log.Debug ("Profile saved");

   exception
      when others =>
         raise GitHub_Profile_Error with "Profile not saved correctly";
   end Save_To_File;

   --------------
   -- Set_SPDX --
   --------------

   procedure Set_SPDX (Profile : in out Profile_Type; SPDX_Id : String) is
      Is_Valid_Id : constant Boolean :=
        Standard.SPDX.Licenses.Valid_Id (SPDX_Id);
   begin
      if Is_Valid_Id then
         Profile.SPDX_Id := To_Unbounded_String (SPDX_Id);
         Log.Debug ("Set_SPDX =" & SPDX_Id);
      else
         raise GitHub_Profile_Error with "Invalid SPDX Id '" & SPDX_Id & "'";
      end if;
   end Set_SPDX;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (Profile : Profile_Type) return Boolean is
     (Profile.Valid_Token);

end GitHub.Profile;
