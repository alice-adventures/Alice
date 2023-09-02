-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with Alice_Alire;
with Alice_Git;
with Alice_User_Config;

separate (Alice_Cmd.Work.Source)

--------------------------------
-- Execute_Init_Project_Euler --
--------------------------------

procedure Execute_Init_Project_Euler is

   package Alr renames Alice_Alire;
   package Cmd renames Alice_Cmd;
   package Dir renames Ada.Directories;
   package Git renames Alice_Git;
   package Usr renames Alice_User_Config;

   Source            : constant String := "Project Euler";
   Repository        : constant String := "project_euler";
   Source_Repository : constant String := "alice-adventures/" & Repository;
   Shared_Repository : constant String :=
     "alice-adventures/" & Repository & "-shared";
   User_Repository   : constant String :=
     Usr.Get_Current_User.Login & "/" & Repository;
begin
   Log.Info ("BEGIN alice source --init " & Repository);

   Log.Info ("Updating indexes");
   Alr.Update_Indexes;

   Log.Detail ("Checking existence of directory " & Repository);
   if Dir.Exists (Repository) then
      declare
         Is_Valid_Dir : Boolean := True;
      begin
         Dir.Set_Directory (Repository);
         if Git.Is_Clone_Of (Source_Repository) then
            if Dir.Exists ("shared") then
               Dir.Set_Directory ("shared");
               if Git.Is_Clone_Of (Shared_Repository) then
                  Log.Info (Source & " already initialized at " & Repository);
               else
                  Log.Error
                    ("Directory " & Dir.Current_Directory &
                     " should be a clone of " & Shared_Repository);
                  Is_Valid_Dir := False;
               end if;
               Dir.Set_Directory ("..");
            end if;
         else
            Log.Error
              ("Directory " & Dir.Current_Directory &
               " should be a clone of " & Source_Repository);
            Is_Valid_Dir := False;
         end if;
         Dir.Set_Directory ("..");
         if not Is_Valid_Dir then
            Cmd.Abort_Execution ("Invalid repository found at " & Repository);
         end if;
      end;
   else
      Log.Info ("Retrieving information from " & Source);
      if Git.Clone_Remote_Repository (Source_Repository) then
         Log.Detail ("Cloned repository " & Source_Repository);
         Log.Info ("Configuring " & Source);
         declare
            CWD : constant String := Dir.Current_Directory;
         begin
            Dir.Set_Directory (Repository);
            if Git.Clone_Remote_Repository (Shared_Repository) then
               Log.Detail ("Cloned repository " & Shared_Repository);
               Dir.Set_Directory (CWD);
            else
               Dir.Set_Directory (CWD);
               Dir.Delete_Tree (Repository);
               Cmd.Abort_Execution
                 ("Could not clone repository " & Shared_Repository);
            end if;
         end;
      else
         Dir.Delete_Tree (Repository);
         Cmd.Abort_Execution
           ("Could not clone repository " & Source_Repository);
      end if;
   end if;

   Log.Detail ("Checking existence of repository " & User_Repository);
   if Git.User_Has_Remote_Repository (Usr.Get_Current_User, Repository) then
      Log.Info ("Repository " & User_Repository & " already exists");
   else
      Log.Info ("Creating repository " & User_Repository);
      if Git.Create_Remote_Repository
          (Usr.Get_Current_User, Repository,
           "Alice Adventures - repository for " & Source & " problems")
      then
         Log.Info ("Repository" & User_Repository & " successfully created");
      else
         Cmd.Abort_Execution
           ("Could not create repository " & User_Repository);
      end if;
   end if;

   Dir.Set_Directory (Repository);
   Log.Info ("Building libraries and tools");
   Alr.Build_Crate;

end Execute_Init_Project_Euler;
