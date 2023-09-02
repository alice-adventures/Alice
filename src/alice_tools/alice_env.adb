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

with Simple_Logging;

package body Alice_Env is

   package Log renames Simple_Logging;

   Alice_Root_Dir : Unbounded_String := To_Unbounded_String ("");

   -------------------------
   -- Is_Alice_Repository --
   -------------------------

   function Is_Alice_Repository (Report_Error : Boolean := True) return Boolean
   is
   begin
      return
        Success : constant Boolean :=
          Alice_Git.Is_Clone_Of (Alice_Repository ("Alice")) do
         if Success then
            Log.Detail ("Alice git repository detected");
         elsif Report_Error then
            Alice_Cmd.Abort_Execution
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
      use Ada.Directories;
      Success : Boolean := False;
   begin
      if not Is_Alice_Repository (Report_Error => False) then
         return False;
      end if;

      Success :=
        Exists ("config") and then Kind ("config") = Directory
        and then Exists
          (Compose
             (Containing_Directory => "config", Name => "alice_config",
              Extension            => "ads"));

      if Success then
         Log.Detail ("found config/alice_config.ads");
         Alice_Root_Dir := To_Unbounded_String (Current_Directory);
         Log.Detail ("Alice_Root_Dir = " & Get_Alice_Root_Dir);
      elsif Report_Error then
         Alice_Cmd.Abort_Execution
           ("'alice' must be invoked from the Alice' root directory");
      end if;

      return Success;
   end Is_Alice_Root_Dir;

   ------------------------
   -- Get_Alice_Root_Dir --
   ------------------------

   function Get_Alice_Root_Dir return String is (To_String (Alice_Root_Dir));

end Alice_Env;
