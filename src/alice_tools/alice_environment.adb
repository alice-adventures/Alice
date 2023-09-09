-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Alice_Cmd;
with Alice_Git;
with Alice_Repository;

with Simple_Logging;

package body Alice_Environment is

   package Cmd renames Alice_Cmd;
   package Dir renames Ada.Directories;
   package Git renames Alice_Git;
   package Log renames Simple_Logging;
   package Repo renames Alice_Repository;

   use all type Dir.File_Kind;

   Alice_Root_Dir : Unbounded_String := Null_Unbounded_String;

   -------------------------
   -- Is_Alice_Repository --
   -------------------------

   function Is_Alice_Repository (Report_Error : Boolean := True) return Boolean
   is
   begin
      return
        Success : constant Boolean := Git.Is_Clone_Of (Repo.Name (Repo.Main))
      do
         if Success then
            Log.Detail ("Alice git repository detected");
         elsif Report_Error then
            Cmd.Abort_Execution
              ("'alice' command must be invoked inside " &
               "the Alice git repository");
         end if;
      end return;
   end Is_Alice_Repository;

   -----------------------
   -- Is_Alice_Root_Dir --
   -----------------------

   function Is_Alice_Root_Dir (Report_Error : Boolean := True) return Boolean
   is
   begin
      if not Is_Alice_Repository (Report_Error => False) then
         return False;
      end if;

      return
        Success : constant Boolean :=
          Dir.Exists ("config") and then Dir.Kind ("config") = Dir.Directory
          and then Dir.Exists
            (Dir.Compose
               (Containing_Directory => "config", Name => "alice_config",
                 Extension           => "ads"))
      do
         if Success then
            Log.Detail ("found config/alice_config.ads");
            Alice_Root_Dir := To_Unbounded_String (Dir.Current_Directory);
            Log.Detail ("Alice_Root_Dir = " & Get_Alice_Root_Dir);
         elsif Report_Error then
            Cmd.Abort_Execution
              ("alice command must be invoked from the Alice root directory");
         end if;
      end return;
   end Is_Alice_Root_Dir;

   ------------------------
   -- Get_Alice_Root_Dir --
   ------------------------

   function Get_Alice_Root_Dir return String is (To_String (Alice_Root_Dir));

end Alice_Environment;
