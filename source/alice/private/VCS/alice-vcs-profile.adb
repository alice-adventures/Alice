-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with SPDX;

package body Alice.VCS.Profile is

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

   ------------------
   -- Save_To_File --
   ------------------

   function Save_To_File
     (Self : Object; File : String) return Alice.Result.Object'Class
   is
      --  This function is a placeholder for saving the profile to a file. The
      --  actual implementation would involve writing the profile data to the
      --  specified file in a format that can be read later. For now, it
      --  simply returns a success result.
   begin
      --  #TODO - Provide a proper implementation
      --  Here you would implement the logic to save the profile to a file.
      --  For example, you could write the profile data to a JSON or YAML
      --  file. This is a stub implementation.
      return Result : Alice.Result.Success_Object;
   end Save_To_File;

end Alice.VCS.Profile;
