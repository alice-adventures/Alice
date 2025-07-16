-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Text_IO;

with SPDX;
with TOML;
with TOML.File_IO;

package body Alice.VCS.Profile is

   --------------------
   -- Create_Profile --
   --------------------

   function Create_Profile
     (Provider   : Alice.VCS.Service.Provider_Name :=
        Alice.VCS.Service.Provider_GitHub;
      Token      : Alice.UString := Alice.Null_UString;
      Login      : Alice.UString := Alice.Null_UString;
      Avatar_URL : Alice.UString := Alice.Null_UString;
      Name       : Alice.UString := Alice.Null_UString;
      Email      : Alice.UString := Alice.Null_UString;
      SPDX_Id    : Alice.UString := Alice.Null_UString) return Object_Access
   is (new Object'
         (Alice.Controlled
          with
            Provider   => Alice.UStr (Provider'Image),
            Token      => Token,
            Login      => Login,
            Avatar_URL => Avatar_URL,
            Name       => Name,
            Email      => Email,
            SPDX_Id    => SPDX_Id));

   ------------------
   -- Get_Provider --
   ------------------

   function Get_Provider (Self : Object) return Alice.VCS.Service.Provider_Name
   is
   begin
      --  #TODO - Review error handling for invalid provider names. In theory
      --  no bad names should be found, but if they are, we should handle them
      --  gracefully.
      return Alice.VCS.Service.Provider_Name'Value (Alice.Str (Self.Provider));
   end Get_Provider;

   ---------------
   -- Get_Token --
   ---------------

   function Get_Token (Self : Object) return String
   is (Alice.Str (Self.Token));

   ---------------
   -- Get_Login --
   ---------------

   function Get_Login (Self : Object) return String
   is (Alice.Str (Self.Login));

   --------------------
   -- Get_Avatar_URL --
   --------------------

   function Get_Avatar_URL (Self : Object) return String
   is (Alice.Str (Self.Avatar_URL));

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Self : Object) return String
   is (Alice.Str (Self.Name));

   ---------------
   -- Get_Email --
   ---------------

   function Get_Email (Self : Object) return String
   is (Alice.Str (Self.Email));

   -----------------
   -- Get_SPDX_Id --
   -----------------

   function Get_SPDX_Id (Self : Object) return String
   is (Alice.Str (Self.SPDX_Id));

   -----------------
   -- Set_SPDX_Id --
   -----------------

   procedure Set_SPDX_Id (Self : in out Object; SPDX_Id : String) is
      --  #TODO - Set the default SPDX ID in the config file.
      Default_SPDX_Id : constant String := "MIT";
      Expression      : constant SPDX.Expression := SPDX.Parse (SPDX_Id);
   begin
      if Expression.Valid then
         Self.SPDX_Id := Alice.UStr (SPDX_Id);
      else
         Self.SPDX_Id := Alice.UStr (Default_SPDX_Id);
      end if;
   end Set_SPDX_Id;

   --------------------
   -- Load_From_File --
   --------------------

   function Load_From_File
     (Self : in out Object; File : String) return Alice.Result.Object'Class is
   begin
      if Ada.Directories.Exists (File) then
         TOML_Result : constant TOML.Read_Result :=
           TOML.File_IO.Load_File (File);

         if TOML_Result.Success then
            if TOML_Result.Value.Has (Key_Login)
              and then TOML_Result.Value.Has (Key_Name)
              and then TOML_Result.Value.Has (Key_Token)
            then
               Self.Provider :=
                 TOML_Result.Value.Get (Key_Provider).As_Unbounded_String;
               Self.Token :=
                 TOML_Result.Value.Get (Key_Token).As_Unbounded_String;
               Self.Login :=
                 TOML_Result.Value.Get (Key_Login).As_Unbounded_String;
               Self.Avatar_URL :=
                 TOML_Result.Value.Get (Key_Avatar_URL).As_Unbounded_String;
               Self.Name :=
                 TOML_Result.Value.Get (Key_Name).As_Unbounded_String;
               Self.Email :=
                 TOML_Result.Value.Get (Key_Email).As_Unbounded_String;
               Self.SPDX_Id :=
                 TOML_Result.Value.Get (Key_SPDX_Id).As_Unbounded_String;
               return Result : Alice.Result.Success_Object;
            else
               return
                 Alice.Result.Create_Error
                   (Alice.Result.Domain,
                    Alice.UStr
                      ("Profile file does not contain "
                       & "a valid VCS profile."));
            end if;
         else
            return
              Alice.Result.Create_Error
                (Alice.Result.Domain,
                 Alice.UStr
                   ("Error loading profile file: "
                    & Alice.Str (TOML_Result.Message)));
         end if;
      else
         return
           Alice.Result.Create_Error
             (Alice.Result.Domain,
              Alice.UStr ("Profile file does not exist: " & File));
      end if;
   end Load_From_File;

   ------------------
   -- Save_To_File --
   ------------------

   function Save_To_File
     (Self : in out Object; File : String) return Alice.Result.Object'Class
   is
      Table      : constant TOML.TOML_Value := TOML.Create_Table;
      Profile_FD : Ada.Text_IO.File_Type;
   begin
      Table.Set (Key_Provider, TOML.Create_String (Self.Provider));
      Table.Set (Key_Token, TOML.Create_String (Self.Token));
      Table.Set (Key_Login, TOML.Create_String (Self.Login));
      Table.Set (Key_Avatar_URL, TOML.Create_String (Self.Avatar_URL));
      Table.Set (Key_Name, TOML.Create_String (Self.Name));
      Table.Set (Key_Email, TOML.Create_String (Self.Email));
      Table.Set (Key_SPDX_Id, TOML.Create_String (Self.SPDX_Id));

      Profile_FD.Create (Ada.Text_IO.Out_File, File);
      TOML.File_IO.Dump_To_File (Table, Profile_FD);
      Profile_FD.Close;

      return Result : Alice.Result.Success_Object;

   exception
      when Ada.Text_IO.Name_Error =>
         return
           Alice.Result.Create_Error
             (Alice.Result.System,
              Alice.UStr ("Could not create profile file: " & File));
      when others =>
         return
           Alice.Result.Create_Error
             (Alice.Result.System,
              Alice.UStr
                ("Unexpected error while saving profile to file: " & File));
   end Save_To_File;

   -------------------
   -- Profile_Image --
   -------------------

   procedure Profile_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Object) is
   begin
      Output.New_Line;
      Output.Put ("(Alice.VCS.Profile.Object with");
      Output.New_Line;
      Output.Increase_Indent;
      Output.Put ("   User_Name   => " & Alice.Str (Value.Name));
      Output.New_Line;
      Output.Put ("   User_Email  => " & Alice.Str (Value.Email));
      Output.New_Line;
      Output.Put ("   User_Login  => " & Alice.Str (Value.Login));
      Output.New_Line;
      Output.Put ("   User_Avatar => " & Alice.Str (Value.Avatar_URL));
      Output.New_Line;
      Output.Put ("   User_Token  => " & Alice.Str (Value.Token));
      Output.New_Line;
      Output.Put ("   SPDX_Id     => " & Alice.Str (Value.SPDX_Id));
      Output.Put (")");
      Output.Decrease_Indent;
      Output.New_Line;
   end Profile_Image;

end Alice.VCS.Profile;
