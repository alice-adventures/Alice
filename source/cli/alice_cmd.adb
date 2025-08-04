-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice_Config;
with Alice.CLI;
with Alice.Context;
with Alice.Env;
with Alice.Std;

procedure Alice_Cmd is
   Context : constant Alice.Context.Object_Access := Alice.Std.Get_Context;
begin
   Context.Log.Initialize;

   case Alice_Config.Log_Level is
      when Alice_Config.Default =>
         Context.Log.Set_Default_Level;

      when Alice_Config.Verbose =>
         Context.Log.Set_Verbose_Level;

      when Alice_Config.Trace =>
         Context.Log.Set_Trace_Level (With_Location_Enabled => True);

      when Alice_Config.Debug =>
         Context.Log.Set_Debug_Level (With_Location_Enabled => True);
   end case;

   --  #TODO - Refuse to run if not in Alice repository

   Alice.CLI.Initialize;
   Alice.CLI.Execute;

   Context.Log.Trace ("Finalization of controlled object");
end Alice_Cmd;
