-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

separate (Alice_Command.Setup)
function Project_Euler return Boolean is
   Alr_Cmd : GNAT.OS_Lib.String_Access;
   Git_Cmd : GNAT.OS_Lib.String_Access;

   Exists_Project_Euler_Repository : constant Boolean :=
     GNAT.OS_Lib.Is_Directory ("project_euler");

   Status : Integer;
begin
   Log.Info ("setup project_euler");

   if Exists_Project_Euler_Repository then
      Log.Error ("Project Euler already setup");
      return False;
   end if;

   Alr_Cmd := GNAT.OS_Lib.Locate_Exec_On_Path ("alr");
   if Alr_Cmd = null then
      Log.Error ("alr cannot be found in PATH");
      return False;
   else
      Log.Debug ("found alr at '" & Alr_Cmd.all & "'");
   end if;

   Git_Cmd := GNAT.OS_Lib.Locate_Exec_On_Path ("git");
   if Git_Cmd = null then
      Log.Error ("git cannot be found in PATH");
      return False;
   else
      Log.Debug ("found git at '" & Git_Cmd.all & "'");
   end if;

   --  clone project-euler
   declare
      Str_Clone         : aliased String := "clone";
      Str_Repository    : aliased String :=
        "git@github.com:alice-adventures/project-euler";
      Str_Project_Euler : aliased String := "project_euler";
   begin
      Log.Detail ("clone project_euler");
      Status :=
        GNAT.OS_Lib.Spawn
          (Git_Cmd.all,
           [Str_Clone'Unchecked_Access, Str_Repository'Unchecked_Access,
           Str_Project_Euler'Unchecked_Access]);
      Log.Debug ("git clone returns" & Status'Image);
      if Status > 0 then
         return False;
      end if;
   end;

   --  update Alire crate
   declare
      Str_Update : aliased String := "update";
   begin
      Log.Detail ("alr update");
      Status := GNAT.OS_Lib.Spawn (Alr_Cmd.all, [Str_Update'Unchecked_Access]);
      Log.Debug ("alr update returns" & Status'Image);
      if Status > 0 then
         return False;
      end if;
   end;

   return True;
end Project_Euler;
