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

with GitHub_API;

with OS_Cmd_Git;

package body Alice_Git is

   package Log renames Simple_Logging;

   --  package Types is new JSON.Types (Integer, Float);
   --  package Parsers is new JSON.Parsers (Types);

   --  use Types;

   -----------------------------
   -- Clone_GitHub_Repository --
   -----------------------------

   function Clone_GitHub_Repository
     (Repo : String; Directory : String := "") return Boolean
   is
      Success    : Boolean;
      Git_Cmd    : OS_Cmd_Git.Cmd_Type;
      Run_Output : OS_Cmd_Git.Run_Output_Type;
   begin
      Git_Cmd.Init;

      Run_Output :=
        Git_Cmd.Run ("clone -q " & GitHub_Root & Repo & " " & Directory);
      Success    := (Run_Output.Return_Code = 0);
      Run_Output.Clean;

      return Success;
   end Clone_GitHub_Repository;

   ------------------------------
   -- Create_GitHub_Repository --
   ------------------------------

   function Create_GitHub_Repository
     (User_Config : User_Config_Type; Repo : String; Description : String)
      return Boolean
   is
   begin
      return
        GitHub_API.Create_A_Repository_For_The_Authenticated_User
          (User_Config, Repo, Description);
   end Create_GitHub_Repository;

   --------------------------------
   -- User_Has_GitHub_Repository --
   --------------------------------

   function User_Has_GitHub_Repository
     (User_Config : User_Config_Type; Repo : String) return Boolean
   is
   begin
      return Exists_GitHub_Repository (User_Config, User_Config.Login, Repo);
   end User_Has_GitHub_Repository;

   ------------------------------
   -- Exists_GitHub_Repository --
   ------------------------------

   function Exists_GitHub_Repository
     (User_Config : User_Config_Type; User : String; Repo : String)
      return Boolean
   is
   begin
      return GitHub_API.Get_A_Repository (User_Config, User, Repo);
   end Exists_GitHub_Repository;

   ------------------
   -- Is_Git_Clone --
   ------------------

   function Is_Git_Clone (Server, Repository : String) return Boolean is
      Cmd_Git    : OS_Cmd_Git.Cmd_Type;
      Run_Output : OS_Cmd_Git.Run_Output_Type;

      Matches : Natural := 0;
      Success : Boolean;

      procedure Origin_Match is
      begin
         Matches := @ + 1;
         Log.Debug ("AWK origin match #" & Matches'Image);
      end Origin_Match;

      procedure Repo_Match is
      begin
         Matches := @ + 1;
         Log.Debug
           ("AWK repo match #" & Matches'Image & ", " & GNAT.AWK.Field (2) &
            " " & GNAT.AWK.Field (3));
      end Repo_Match;

   begin
      Cmd_Git.Init;

      Run_Output := Cmd_Git.Run ("remote -v");
      declare
         HTTPS_Origin_Matcher : constant GNAT.Regpat.Pattern_Matcher :=
           GNAT.Regpat.Compile
             ("^https://" & Server & "/" & Repository & "\.git$");
         SSH_Origin_Matcher   : constant GNAT.Regpat.Pattern_Matcher :=
           GNAT.Regpat.Compile
             ("^git@" & Server & ":" & Repository & "\.git$");
      begin
         GNAT.AWK.Add_File (Run_Output.Temp_File.all);
         GNAT.AWK.Register (1, "origin", Origin_Match'Unrestricted_Access);
         GNAT.AWK.Register
           (2, HTTPS_Origin_Matcher, Repo_Match'Unrestricted_Access);
         GNAT.AWK.Register
           (2, SSH_Origin_Matcher, Repo_Match'Unrestricted_Access);

         GNAT.AWK.Parse;
         GNAT.AWK.Close (GNAT.AWK.Default_Session.all);

      end;
      Run_Output.Clean;

      Success := (Matches = 4);
      if Success then
         Log.Detail ("alice git repository detected");
      else
         Log.Debug ("'alice' must be invoked inside the alice git repository");
      end if;

      return Success;
   end Is_Git_Clone;

end Alice_Git;
