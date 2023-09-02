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

   function Clone_Remote_Repository
     (Repository : String; Directory : String := "") return Boolean
   is
      Success    : Boolean;
      Git_Cmd    : OS_Cmd_Git.Cmd_Type;
      Run_Output : OS_Cmd_Git.Run_Output_Type;
   begin
      Git_Cmd.Init;

      Run_Output :=
        Git_Cmd.Run
          ("clone -q " & Env.Remote_Repo_Root & Repository & " " & Directory);
      Success    := (Run_Output.Return_Code = 0);
      Run_Output.Clean;

      return Success;
   end Clone_Remote_Repository;

   ------------------------------
   -- Create_Remote_Repository --
   ------------------------------

   function Create_Remote_Repository
     (User_Config : User_Config_Type; Repository : String;
      Description : String) return Boolean
   is
   begin
      return
        GitHub_API.Create_A_Repository_For_The_Authenticated_User
          (User_Config, Repository, Description);
   end Create_Remote_Repository;

   --------------------------------
   -- User_Has_Remote_Repository --
   --------------------------------

   function User_Has_Remote_Repository
     (User_Config : User_Config_Type; Repository : String) return Boolean
   is
   begin
      return
        Exists_Remote_Repository (User_Config, User_Config.Login, Repository);
   end User_Has_Remote_Repository;

   ------------------------------
   -- Exists_Remote_Repository --
   ------------------------------

   function Exists_Remote_Repository
     (User_Config : User_Config_Type; User : String; Repository : String)
      return Boolean
   is
   begin
      return GitHub_API.Get_A_Repository (User_Config, User, Repository);
   end Exists_Remote_Repository;

   ---------------------
   -- Is_Git_Clone_Of --
   ---------------------

   function Is_Clone_Of
     (Repository : String; Server : String := Env.Remote_Repo) return Boolean
   is
      Cmd_Git    : OS_Cmd_Git.Cmd_Type;
      Run_Output : OS_Cmd_Git.Run_Output_Type;

      Matches : Natural := 0;
      Success : Boolean;

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
      Cmd_Git.Init;

      Log.Debug ("Is clone of " & Server & ":" & Repository);
      Run_Output := Cmd_Git.Run ("remote -v");
      declare
         AWK_Session          : GNAT.AWK.Session_Type;
         HTTPS_Origin_Matcher : constant GNAT.Regpat.Pattern_Matcher :=
           GNAT.Regpat.Compile
             ("^https://" & Server & "/" & Repository & "(?:\.git)?$");
         SSH_Origin_Matcher   : constant GNAT.Regpat.Pattern_Matcher :=
           GNAT.Regpat.Compile
             ("^git@" & Server & ":" & Repository & "(?:\.git)?$");
      begin
         GNAT.AWK.Set_Current (AWK_Session);
         GNAT.AWK.Add_File (Run_Output.Temp_File.all);
         GNAT.AWK.Register (1, "origin", Origin_Match'Unrestricted_Access);
         GNAT.AWK.Register
           (2, HTTPS_Origin_Matcher, Repo_Match'Unrestricted_Access);
         GNAT.AWK.Register
           (2, SSH_Origin_Matcher, Repo_Match'Unrestricted_Access);

         GNAT.AWK.Parse;
         GNAT.AWK.Close (AWK_Session);
         --  GNAT.AWK.Close (GNAT.AWK.Default_Session.all);

      end;
      Run_Output.Clean;

      Success := (Matches = 4);
      if Success then
         Log.Detail (Server & ":" & Repository & " repository detected");
      else
         Log.Debug (Server & ":" & Repository & " repository not found");
      end if;

      return Success;
   end Is_Clone_Of;

end Alice_Git;
