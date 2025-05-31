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

   Local_Profile : aliased Profile_Type;
   Valid_Profile : Boolean := False;

   -----------------------
   -- Has_Local_Profile --
   -----------------------

   function Has_Local_Profile return Boolean is
      Path : GNAT.OS_Lib.String_Access;
   begin
      Path :=
        GNAT.OS_Lib.Locate_Regular_File
          (Conf.Local.Profile, Conf.Local.Config_Directory);

      return Success : constant Boolean := (Path /= null) do
         if Success then
            Log.Detail ("local profile file found at " & Path.all);
            GNAT.OS_Lib.Free (Path);
         else
            Log.Detail ("local profile file not found");
         end if;
      end return;

   exception
      when others =>
         Log.Error ("internal OS error");
         return False;
   end Has_Local_Profile;

   -----------------------
   -- Get_Local_Profile --
   -----------------------

   function Get_Local_Profile
     (Refresh_From_File : Boolean := False) return Profile_Type is
   begin
      if Refresh_From_File or else not Valid_Profile then
         Local_Profile.Load_From_File;
         Valid_Profile := True;
      end if;
      return Local_Profile;

   exception
      when Error : GitHub_Profile_Error =>
         Log.Error ("error loading profile");
         raise Participant_Error with Error.Exception_Message;
      when others =>
         Log.Error ("undefined error loading profile");
         raise Participant_Error with "Undefined error loading profile";
   end Get_Local_Profile;

   ------------------------
   -- Save_Local_Profile --
   ------------------------

   procedure Save_Local_Profile (Local_Profile : Profile_Type) is
   begin
      Local_Profile.Save_To_File;
   end Save_Local_Profile;

   -----------------------------
   -- Load_Local_Profile_Into --
   -----------------------------

   procedure Load_Local_Profile_Into (Remote_Profile : in out Profile_Type) is
      Profile : Profile_Type;
   begin
      Profile.Load_From_File;
      Remote_Profile := Profile;

   exception
      when Error : GitHub_Profile_Error =>
         Log.Error ("error loading profile");
         raise Participant_Error with Error.Exception_Message;
      when others =>
         Log.Error ("undefined error loading profile");
         raise Participant_Error with "Undefined error loading profile";
   end Load_Local_Profile_Into;

   -----------------------------
   -- Load_Profile_From_Token --
   -----------------------------

   procedure Load_Profile_From_Token
     (Profile : in out Profile_Type; Token : String) is
   begin
      Log.Info ("Retrieving information from token");
      Profile.Load_From_GitHub_Token (Token);
      Profile.Load_From_Git_Config;
   end Load_Profile_From_Token;

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
