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
     (User_Name   : Alice.UString := Alice.Null_UString;
      User_Email  : Alice.UString := Alice.Null_UString;
      User_Login  : Alice.UString := Alice.Null_UString;
      User_Avatar : Alice.UString := Alice.Null_UString;
      User_Token  : Alice.UString := Alice.Null_UString;
      SPDX_Id     : Alice.UString := Alice.Null_UString) return Object_Access
   is (new Object'
         (Alice.Controlled
          with
            User_Name   => User_Name,
            User_Email  => User_Email,
            User_Login  => User_Login,
            User_Avatar => User_Avatar,
            User_Token  => User_Token,
            SPDX_Id     => SPDX_Id));

   -------------------
   -- Get_User_Name --
   -------------------

   function Get_User_Name (Self : Object) return String
   is (Alice.Str (Self.User_Name));

   --------------------
   -- Get_User_Email --
   --------------------

   function Get_User_Email (Self : Object) return String
   is (Alice.Str (Self.User_Email));

   --------------------
   -- Get_User_Login --
   --------------------

   function Get_User_Login (Self : Object) return String
   is (Alice.Str (Self.User_Login));

   ---------------------
   -- Get_User_Avatar --
   ---------------------

   function Get_User_Avatar (Self : Object) return String
   is (Alice.Str (Self.User_Avatar));

   --------------------
   -- Get_User_Token --
   --------------------

   function Get_User_Token (Self : Object) return String
   is (Alice.Str (Self.User_Token));

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
               Self.User_Name :=
                 TOML_Result.Value.Get (Key_Name).As_Unbounded_String;
               Self.User_Email :=
                 TOML_Result.Value.Get (Key_Email).As_Unbounded_String;
               Self.User_Login :=
                 TOML_Result.Value.Get (Key_Login).As_Unbounded_String;
               Self.User_Avatar :=
                 TOML_Result.Value.Get (Key_Avatar_URL).As_Unbounded_String;
               Self.User_Token :=
                 TOML_Result.Value.Get (Key_Token).As_Unbounded_String;
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
      Table.Set (Key_Name, TOML.Create_String (Self.User_Name));
      Table.Set (Key_Email, TOML.Create_String (Self.User_Email));
      Table.Set (Key_Login, TOML.Create_String (Self.User_Login));
      Table.Set (Key_Token, TOML.Create_String (Self.User_Avatar));
      Table.Set (Key_Token, TOML.Create_String (Self.User_Token));
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
              Alice.UStr ("Unexpected error while saving profile to file: "
                           & File));
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
      Output.Put ("   User_Name   => " & Alice.Str (Value.User_Name));
      Output.New_Line;
      Output.Put ("   User_Email  => " & Alice.Str (Value.User_Email));
      Output.New_Line;
      Output.Put ("   User_Login  => " & Alice.Str (Value.User_Login));
      Output.New_Line;
      Output.Put ("   User_Avatar => " & Alice.Str (Value.User_Avatar));
      Output.New_Line;
      Output.Put ("   User_Token  => " & Alice.Str (Value.User_Token));
      Output.New_Line;
      Output.Put ("   SPDX_Id     => " & Alice.Str (Value.SPDX_Id));
      Output.Put (")");
      Output.Decrease_Indent;
      Output.New_Line;
   end Profile_Image;

end Alice.VCS.Profile;
