-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;   use Ada.Directories;
with Alice_Alire;       use Alice_Alire;
with Alice_Git;         use Alice_Git;
with Alice_User_Config; use Alice_User_Config;

separate (Alice_Cmd.Work.Source)

--------------------------------
-- Execute_Init_Project_Euler --
--------------------------------

procedure Execute_Init_Project_Euler is
   Repository_Name : constant String :=
     Get_Current_User.Author & "/project_euler";
begin
   Log.Info ("Updating indexes");
   Update_Indexes;

   if not User_Has_GitHub_Repository (Get_Current_User) then
      Log.Info
        ("Creating repository " & Get_Current_User.Author & "/" &
         "project_euler");
      Create_GitHub_Repository
        (Get_Current_User, "project_euler",
         "Alice Adventures - repository for Project Euler problems");
   end if;

   if Exists ("project_euler") then
      Set_Directory ("project_euler");
      if Is_Git_Clone_Of ("github.com", "alice-adventures/project_euler") then
         Log.Info ("Project Euler already initialized");
      else
         Abort_Execution ("Invalid repository found at project_euler");
      end if;
   else
      Log.Info ("Retrieving information from Project Euler");
      if Clone_GitHub_Repository ("alice-adventures/project_euler") then
         Log.Info ("Configuring Project Euler");
         Set_Directory ("project_euler");
      else
         Abort_Execution ("Could not clone project_euler repository");
      end if;
   end if;

   Log.Info ("Building libraries and tools");
   Build_Crate;

end Execute_Init_Project_Euler;
