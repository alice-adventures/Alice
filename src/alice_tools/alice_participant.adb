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

   Current_Profile : aliased Profile_Type;
   Valid_Profile   : Boolean := False;

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

   procedure Load_Profile is
   begin
      Current_Profile.Load_From_File;
      Valid_Profile := True;
   end Load_Profile;

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
   begin
      Profile.Save_To_File;
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
         Current_Profile.Load_From_File;
         Valid_Profile := True;
      end if;
      return Current_Profile;
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
