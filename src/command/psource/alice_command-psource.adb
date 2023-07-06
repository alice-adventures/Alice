-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice_Setup;

with GNAT.OS_Lib;
with Simple_Logging;

use all type GNAT.OS_Lib.String_Access;

package body Alice_Command.PSource is

   package Log renames Simple_Logging;

   function Project_Euler return Boolean is separate;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Cmd : in out Cmd_Type; Args : AAA.Strings.Vector)
   is
      Args_Length : constant Natural := Natural (Args.Length);

      Exists_Setup_Directory : constant Boolean :=
        GNAT.OS_Lib.Is_Directory (".setup");
   begin
      if not Exists_Setup_Directory then
         Log.Detail ("setup directory not found");
         if not Alice_Setup.Initialize then
            Log.Always ("Alice could not be configured, aborting");
            return;
         end if;
      else
         Log.Detail ("alice already setup");
      end if;

      if Args_Length = 0 then
         Log.Error ("Too few arguments: Problem Source required");
         return;
      end if;

      if Args_Length > 1 then
         Log.Error ("Too many arguments");
         return;
      end if;

      declare
         Problem_Source : constant String := Args.First_Element;
         Success        : Boolean;
      begin
         pragma Unreferenced (Success);
         if Problem_Source = "project_euler" then
            Success := PSource.Project_Euler;
            Log.Always ("Project Euler successfully setup");
         else
            Log.Error ("Unknown Problem Source '" & Problem_Source & "'");
         end if;
      end;
   end Execute;

end Alice_Command.PSource;
