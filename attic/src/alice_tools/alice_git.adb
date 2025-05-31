-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GNAT.AWK;
with GNAT.Regpat;

with Simple_Logging;

with Alice_Repository;
with GitHub.API;
with OS_Cmd_Git;
with Protocols;

package body Alice_Git is
   --
   package GHub renames GitHub.API;
   package Log renames Simple_Logging;
   package Repo renames Alice_Repository;
   package Prot renames Protocols;

   -----------------------------
   -- Clone_Remote_Repository --
   -----------------------------

   function Clone_Remote_Repository
     (Repository : String; Directory : String := "") return Boolean
   is
      Git_Cmd    : OS_Cmd_Git.Cmd_Type;
      Run_Output : OS_Cmd_Git.Run_Output_Type;
   begin
      return Success : Boolean do
         Git_Cmd.Init;

         Run_Output :=
           Git_Cmd.Run ("clone -q " & Repo.URL (Repository) & " " & Directory);
         Success    := (Run_Output.Return_Code = 0);
         Run_Output.Clean;
      end return;
   end Clone_Remote_Repository;

   ------------------------------
   -- Create_Remote_Repository --
   ------------------------------

   function Create_Remote_Repository
     (Profile : Profile_Type; Repository : String;
      Description : String) return Boolean
   is
   begin
      --  return
      GHub.Create_A_Repository_For_The_Authenticated_User
        (Profile, Repository, Description);
      return True;
   end Create_Remote_Repository;

   --------------------------------
   -- User_Has_Remote_Repository --
   --------------------------------

   function User_Has_Remote_Repository
     (Profile : Profile_Type; Repository : String) return Boolean
   is
   begin
      Log.Detail
         ("Checking if User " & Profile.Login &
          " has remote Repository " & Repository);
      return
        Exists_Remote_Repository (Profile, Profile.Login, Repository);
   end User_Has_Remote_Repository;

   ------------------------------
   -- Exists_Remote_Repository --
   ------------------------------

   function Exists_Remote_Repository
     (Profile : Profile_Type; User : String; Repository : String)
      return Boolean
   is
   begin
      Log.Detail
         ("Checking if remote Repository " &
           Repository & " exists for User " & User);
      return GHub.Get_A_Repository (Profile, User, Repository);
   end Exists_Remote_Repository;

   ---------------------
   -- CWD_Is_Clone_Of --
   ---------------------

   function CWD_Is_Clone_Of (Repository : String) return Boolean is
      Cmd_Git    : OS_Cmd_Git.Cmd_Type;
      Run_Output : OS_Cmd_Git.Run_Output_Type;

      Matches : Natural := 0;

      procedure Origin_Match is
      begin
         Matches := @ + 1;
         Log.Debug
           ("AWK 'origin' match #" & Matches'Image & ": " &
            GNAT.AWK.Field (0));
      end Origin_Match;

      procedure Repo_Match is
      begin
         Matches := @ + 1;
         Log.Debug
           ("AWK repo match #" & Matches'Image & ", " & GNAT.AWK.Field (2) &
            " " & GNAT.AWK.Field (3));
      end Repo_Match;

   begin
      Log.Debug ("Is clone of " & Repository);

      Cmd_Git.Init;
      Run_Output := Cmd_Git.Run ("remote -v");
      declare
         AWK_Session          : GNAT.AWK.Session_Type;
         HTTPS_Origin_Matcher : constant GNAT.Regpat.Pattern_Matcher :=
           GNAT.Regpat.Compile
             ("^" & Repo.URL (Repository, Prot.https) & "(?:\.git)?$");
         Git_Origin_Matcher   : constant GNAT.Regpat.Pattern_Matcher :=
           GNAT.Regpat.Compile
             ("^" & Repo.URL (Repository, Prot.git) & "(?:\.git)?$");
      begin
         GNAT.AWK.Set_Current (AWK_Session);
         GNAT.AWK.Add_File (Run_Output.Temp_File.all);
         GNAT.AWK.Register (1, "origin", Origin_Match'Unrestricted_Access);
         GNAT.AWK.Register
           (2, HTTPS_Origin_Matcher, Repo_Match'Unrestricted_Access);
         GNAT.AWK.Register
           (2, Git_Origin_Matcher, Repo_Match'Unrestricted_Access);

         GNAT.AWK.Parse;
         GNAT.AWK.Close (AWK_Session);
      end;
      Run_Output.Clean;

      return Success : constant Boolean := (Matches = 4) do
         Log.Detail
           (Repo.URL (Repository) &
            (if Success then " repository detected"
             else " repository not found"));
      end return;
   end CWD_Is_Clone_Of;

end Alice_Git;
