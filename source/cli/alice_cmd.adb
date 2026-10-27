-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.Result;
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

   Alice.CLI.Initialize;

   if Alice.Env.CWD_Is_Alice_Repository then
      Context.Log.Info ("Operating inside an Alice repository");
   else
      Context.Log.Info ("Not operating inside an Alice repository");
      Context.Err.Exit_Application
        (Error_Level => Alice.Result.System,
         Explain     =>
           Alice.UStr
             ("This command must be run inside an Alice repository."));
   end if;

   Alice.CLI.Execute;

   Context.Log.Trace ("Finalization of controlled object");
end Alice_Cmd;
