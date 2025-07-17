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

   use all type Alice.VCS.Service.Name.Enum;

   --------------------
   -- Create_Profile --
   --------------------

   function Create_Profile
     (Service    : Alice.VCS.Service.Name.Enum;
      Token      : Alice.UString;
      Login      : Alice.UString;
      Avatar_URL : Alice.UString := Alice.Null_UString;
      Name       : Alice.UString := Alice.Null_UString;
      Email      : Alice.UString := Alice.Null_UString;
      SPDX_Id    : Alice.UString := Alice.Null_UString) return Object_Access
   is (new Object'
         (Alice.Controlled
          with
            Service    => Alice.UStr (Service'Image),
            Token      => Token,
            Login      => Login,
            Avatar_URL => Avatar_URL,
            Name       => Name,
            Email      => Email,
            SPDX_Id    => SPDX_Id));

   -----------------
   -- Get_Service --
   -----------------

   function Get_Service (Self : Object) return Alice.VCS.Service.Name.Enum is
   begin
      return
         Name : constant Alice.VCS.Service.Name.Enum :=
           Alice.VCS.Service.Name.Enum'Value (Alice.Str (Self.Service));
   exception
      when Constraint_Error =>
         return Alice.VCS.Service.Name.None;
   end Get_Service;

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
            if TOML_Result.Value.Has (Key_Service)
              and then TOML_Result.Value.Has (Key_Token)
              and then TOML_Result.Value.Has (Key_Login)
              and then TOML_Result.Value.Has (Key_Avatar_URL)
              and then TOML_Result.Value.Has (Key_Name)
              and then TOML_Result.Value.Has (Key_Email)
              and then TOML_Result.Value.Has (Key_SPDX_Id)
            then
               Self.Service :=
                 TOML_Result.Value.Get (Key_Service).As_Unbounded_String;
               if Self.Get_Service = Alice.VCS.Service.Name.None then
                  return
                    Alice.Result.Create_Error
                      (Alice.Result.Domain,
                       Alice.UStr
                         ("Invalid service name '"
                          & Alice.Str (Self.Service)
                          & "' in file '"
                          & File
                          & "'"));
               end if;

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
                      ("Invalid profile, some keys missing in file '"
                       & File
                       & "'"));
            end if;
         else
            return
              Alice.Result.Create_Error
                (Alice.Result.Domain,
                 Alice.UStr
                   ("Error '"
                    & Alice.Str (TOML_Result.Message)
                    & "' in file '"
                    & File
                    & "'"));
         end if;
      else
         return
           Alice.Result.Create_Error
             (Alice.Result.Domain,
              Alice.UStr ("Profile file '" & File & "' does not exist"));
      end if;

   exception
      when E : others =>
         return
           Alice.Result.Create_Error
             (Alice.Result.System, Alice.UStr (E.Exception_Message));
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
      Table.Set (Key_Service, TOML.Create_String (Self.Service));
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
      Output.Put (Key_Service & "    => " & Alice.Str (Value.Service));
      Output.New_Line;
      Output.Put (Key_Token & "      => " & Alice.Str (Value.Token));
      Output.New_Line;
      Output.Put (Key_Login & "      => " & Alice.Str (Value.Login));
      Output.New_Line;
      Output.Put (Key_Avatar_URL & " => " & Alice.Str (Value.Avatar_URL));
      Output.New_Line;
      Output.Put (Key_Name & "       => " & Alice.Str (Value.Name));
      Output.New_Line;
      Output.Put (Key_Email & "      => " & Alice.Str (Value.Email));
      Output.New_Line;
      Output.Put (Key_SPDX_Id & "    => " & Alice.Str (Value.SPDX_Id));
      Output.Put (")");
      Output.Decrease_Indent;
      Output.New_Line;
   end Profile_Image;

end Alice.VCS.Profile;
