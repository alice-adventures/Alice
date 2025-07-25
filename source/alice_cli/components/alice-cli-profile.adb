-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO;

with SPDX;
with SPDX.Licenses;

with Alice.IFace.Error_Handler; use Alice.IFace.Error_Handler; -- for Hints

with Alice.Config;
with Alice.Result;
with Alice.VCS.Profile;
with Alice.VCS.Profile.Result;
with Alice.VCS.Service.GitHub;

package body Alice.CLI.Profile is

   use all type Alice.Result.Status_Type;

   --------------------
   -- Setup_Switches --
   --------------------

   overriding
   procedure Setup_Switches
     (Self   : in out Object;
      Config : in out CLIC.Subcommand.Switches_Configuration) is
   begin
      --!pp off
      CLIC.Subcommand.Define_Switch
        (Config      => Config,
         Output      => Self.Flag.Token'Access,
         Switch      => "-t",
         Long_Switch => "--token",
         Help        => "Set member profile from GitHub token");

      CLIC.Subcommand.Define_Switch
        (Config      => Config,
         Output      => Self.Flag.SPDX'Access,
         Switch      => "-s",
         Long_Switch => "--spdx",
         Help        => "Set license SPDX ID for your work");

      CLIC.Subcommand.Define_Switch
        (Config      => Config,
         Output      => Self.Flag.Refresh'Access,
         Switch      => "-r",
         Long_Switch => "--refresh",
         Help        => "Refresh member configuration");
   --!pp on
   end Setup_Switches;

   ------------------
   -- Execute_Show --
   ------------------

   procedure Execute_Show (Self : in out Object) is
      Profile : Alice.VCS.Profile.Object;
      Result  : constant Alice.Result.Object'Class :=
        Profile.Load_From_File (Alice.Config.Local.Profile);
   begin
      if Result.Status = Alice.Result.Success then
         Ada.Text_IO.Put_Line (Profile.To_String);
      else
         Self.Context.Err.Exit_Application (Result);
      end if;
   end Execute_Show;

   -------------------
   -- Execute_Token --
   -------------------

   procedure Execute_Token (Self : in out Object; Token : String) is
      GitHub         : Alice.VCS.Service.GitHub.Object;
      Profile_Result : Alice.VCS.Profile.Result.Object'Class :=
        GitHub.Get_Member_Profile_From_Token (Token);
   begin
      if Profile_Result.Status = Alice.Result.Success then
         Profile : constant Alice.VCS.Profile.Object_Access :=
           Profile_Result.Get_Profile;
         Save_Result : constant Alice.Result.Object'Class :=
           Profile.Save_To_File (Alice.Config.Local.Profile);
         if Save_Result.Status = Alice.Result.Error then
            Self.Context.Err.Exit_Application
              (Save_Result, Alice.UStr (Hint.File_Write_Error));
         end if;
      else
         Self.Context.Err.Exit_Application (Profile_Result);
      end if;
   end Execute_Token;

   ------------------
   -- Execute_SPDX --
   ------------------

   function Execute_SPDX (Self : in out Object; SPDX_Id : String) return String
   is
   begin
      if SPDX_Id = "list" then
         Ada.Text_IO.Put_Line ("Available SPDX license identifiers:");
         Ada.Text_IO.New_Line;

         --!pp off
         Ada.Text_IO.Put_Line
           ("Identifier (case sensitive)" &
            ASCII.CR & ASCII.HT & ASCII.HT & ASCII.HT & ASCII.HT & ASCII.HT
            & "License");
         --!pp on

         Ada.Text_IO.Put_Line
           ("------------------------------------    "
            & "--------------------------------------"
            & "--------------------------------------");

         for Id in SPDX.Licenses.Id'First .. SPDX.Licenses.Id'Last loop
         --!pp off
            Ada.Text_IO.Put_Line
              (SPDX.Licenses.Img_Ptr (Id).all &
               ASCII.CR & ASCII.HT & ASCII.HT & ASCII.HT & ASCII.HT & ASCII.HT
               & SPDX.Licenses.Name_Ptr (Id).all);
         --!pp on
         end loop;

         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line
           ("Use 'alice config --license <SPDX Id>' to set it.");
         return "";
      end if;

      Valid_SPDX_Id : constant Boolean := SPDX.Valid (SPDX.Parse (SPDX_Id));
      if not Valid_SPDX_Id then
         Self.Context.Log.Warning
           ("Invalid SPDX ID '"
            & SPDX_Id
            & "'"
            & ", set to default '"
            & Alice.VCS.Profile.Default_SPDX_Id
            & "'"
            & " instead");
      end if;

      Profile : Alice.VCS.Profile.Object;
      Load_Result : constant Alice.Result.Object'Class :=
        Profile.Load_From_File (Alice.Config.Local.Profile);

      if Load_Result.Status = Alice.Result.Success then
         Real_SPDX_Id : constant String :=
           (if Valid_SPDX_Id
            then SPDX_Id
            else Alice.VCS.Profile.Default_SPDX_Id);

         Profile.Set_SPDX_Id (Real_SPDX_Id);
         Save_Result : constant Alice.Result.Object'Class :=
           Profile.Save_To_File (Alice.Config.Local.Profile);

         if Save_Result.Status = Alice.Result.Success then
            return Real_SPDX_Id;
         else
            Self.Context.Err.Exit_Application (Save_Result);
         end if;
      else
         Self.Context.Err.Exit_Application (Load_Result);
      end if;
      return "";
   end Execute_SPDX;

   ---------------------
   -- Execute_Refresh --
   ---------------------

   procedure Execute_Refresh (Self : in out Object) is
      Profile : Alice.VCS.Profile.Object;
      Result  : constant Alice.Result.Object'Class :=
        Profile.Load_From_File (Alice.Config.Local.Profile);
   begin
      if Result.Status = Alice.Result.Success then
         Token : constant String := Profile.Get_Token;
         SPDX_Id : constant String := Profile.Get_SPDX_Id;
         Self.Execute_Token (Token);
         if Self.Execute_SPDX (SPDX_Id)'Length > 0 then
            null;
         end if;
      else
         Self.Context.Err.Exit_Application (Result);
      end if;
   end Execute_Refresh;

   -------------
   -- Execute --
   -------------

   overriding
   procedure Execute (Self : in out Object; Args : AAA.Strings.Vector) is
      Flags_Count : Natural;
      Args_Count  : constant Natural := Natural (Args.Length);
   begin
      Flags_Count :=
        (if Self.Flag.Refresh then 1 else 0)
        + (if Self.Flag.Token then 1 else 0)
        + (if Self.Flag.SPDX then 1 else 0);

      if Flags_Count = 0 then
         Self.Flag.Show := True;
      elsif Flags_Count > 1 then
         Self.Context.Err.Exit_Application
           (Alice.Result.Domain,
            Alice.UStr
              ("Specify only one of the flags: "
               & "--token, --spdx or --refresh"));
      end if;

      Invalid_Args : constant Alice.Result.Error_Object'Class :=
        Alice.Result.Create_Error
          (Alice.Result.Domain, Alice.UStr (Hint.Invalid_Args));

      if Self.Flag.Show then
         Self.Execute_Show;
      elsif Self.Flag.Token then
         if Args_Count = 1 then
            Self.Execute_Token (Args.First_Element);
            Ada.Text_IO.Put_Line
              ("Profile saved to " & Alice.Config.Local.Profile);
         else
            Self.Context.Err.Exit_Application
              (Invalid_Args, Alice.UStr ("--token requires one argument"));
         end if;
      elsif Self.Flag.SPDX then
         if Args_Count = 1 then
            SPDX_Id : constant String :=
              Self.Execute_SPDX (Args.First_Element);
            if SPDX_Id'Length > 0 then
               Ada.Text_IO.Put_Line ("SPDX Id updated to '" & SPDX_Id & "'");
            end if;
         else
            Self.Context.Err.Exit_Application
              (Invalid_Args, Alice.UStr ("--spdx requires one argument"));
         end if;
      elsif Self.Flag.Refresh then
         if Args_Count = 0 then
            Self.Execute_Refresh;
            Ada.Text_IO.Put_Line
              ("New profile saved to " & Alice.Config.Local.Profile);
         else
            Self.Context.Err.Exit_Application
              (Invalid_Args, Alice.UStr ("--refresh requires no argument"));
         end if;
      end if;

   end Execute;

end Alice.CLI.Profile;
