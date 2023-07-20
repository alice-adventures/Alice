-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories; use Ada.Directories;
with Alice_Alire;     use Alice_Alire;
with Alice_Git;       use Alice_Git;

separate (Alice_Cmd.Work.Source)

--------------------------------
-- Execute_Init_Project_Euler --
--------------------------------

procedure Execute_Init_Project_Euler is
begin
   if Exists ("project_euler") then
      Set_Directory ("project_euler");
      if Is_Git_Clone_Of ("github.com", "alice-adventures/project_euler") then
         Log.Info ("Project Euler already initialized");
         return;
      else
         Abort_Execution ("Invalid repository found at project_euler");
      end if;
   end if;

   Log.Info ("Retrieving information for Project Euler");
   if Clone_GitHub_Repository ("alice-adventures/project_euler") then
      Log.Info ("Configuring Project Euler");
   else
      Abort_Execution ("Could not clone project_euler repository");
   end if;

   Log.Info ("Updating contents");
   Update_Indexes;

   Set_Directory ("project_euler");

   Log.Info ("Building libraries and tools");
   Build_Crate;

end Execute_Init_Project_Euler;
