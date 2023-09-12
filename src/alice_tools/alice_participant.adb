-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GNAT.OS_Lib;

with Simple_Logging;

with Alice_Configuration;

package body Alice_Participant is

   package Conf renames Alice_Configuration;
   package Log renames Simple_Logging;

   use all type GNAT.OS_Lib.String_Access;

   Profile       : aliased Profile_Type;
   Valid_Profile : Boolean := False;

   -----------------
   -- Has_Profile --
   -----------------

   function Has_Profile return Boolean is
      Path : GNAT.OS_Lib.String_Access;
   begin
      Path :=
        GNAT.OS_Lib.Locate_Regular_File
          (Conf.Local.Config_File, Conf.Local.Config_Directory);

      return Success : constant Boolean := (Path /= null) do
         if Success then
            Log.Detail ("profile file found at " & Path.all);
            GNAT.OS_Lib.Free (Path);
         else
            Log.Detail ("user profile file not found,");
         end if;
      end return;
   end Has_Profile;

   ------------------
   -- Load_Profile --
   ------------------

   procedure Load_Profile (Profile : in out Profile_Type) is
   begin
      if not Has_Profile then
         raise Participant_Error with "Profile not found";
      end if;
      Profile.Load_From_File;
   end Load_Profile;

   ------------------
   -- Save_Profile --
   ------------------

   procedure Save_Profile (Profile : Profile_Type) is
   --  use Ada.Directories;
   --  Success     : constant Boolean         := True;
   --  Table       : constant TOML.TOML_Value := TOML.Create_Table;
   --  Config_File : Ada.Text_IO.File_Type;
   begin
      Profile.Save_To_File;
      --  Table.Set
      --    (Key_Login, TOML.Create_String (To_String (Profile.Login)));
      --  Table.Set (Key_Name, TOML.Create_String (To_String (Profile.Name)));
      --  Table.Set
      --    (Key_Email, TOML.Create_String (To_String (Profile.Email)));
      --  Table.Set
      --    (Key_Token, TOML.Create_String (To_String (Profile.Token)));
      --  Table.Set
      --    (Key_SPDX_Id, TOML.Create_String (To_String (Profile.SPDX_Id)));

      --  Config_File.Create
      --    (Ada.Text_IO.Out_File,
      --     Compose
      --       (Containing_Directory => Conf.Local.Config_Directory,
      --        Name                 => Conf.Local.Config_File));

      --  TOML.File_IO.Dump_To_File (Table, Config_File);

      --  Config_File.Close;
      --  return Success;
   end Save_Profile;

   ----------------------------
   -- Set_Profile_From_Token --
   ----------------------------

   procedure Set_Profile_From_Token
     (Profile : in out Profile_Type; Token : String)
   is
   begin
      Log.Info ("Retrieving information from token");
      Profile.Load_From_GitHub_Token (Token);
      Profile.Load_From_Git_Config;
   end Set_Profile_From_Token;

   -----------------------------
   -- Get_Current_Participant --
   -----------------------------

   function Get_Current_Participant return Profile_Type is
   begin
      if not Valid_Profile then
         Profile.Load_From_File;
         Valid_Profile := True;
      end if;
      return Profile;
   end Get_Current_Participant;

   ----------
   -- Show --
   ----------

   procedure Show (Profile : Profile_Type) is
   begin
      Log.Always ("   name    : " & Profile.Name);
      Log.Always ("   email   : " & Profile.Email);
      Log.Always ("   login   : " & Profile.Login);
      Log.Always
        ("   token   : " & (if Profile.Is_Valid then "VALID" else "INVALID"));
      Log.Always ("   spdx_id : " & Profile.SPDX);
   end Show;

end Alice_Participant;
