-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.CLI;
with Alice.Context;
with Alice.Env;
with Alice.Std;

procedure Alice_Cmd is
   Context : constant Alice.Context.Object_Access := Alice.Std.Get_Context;
begin
   Alice.CLI.Initialize;
   Alice.CLI.Execute;

   Context.Log.Debug (Alice.Env.New_Line & "Context => " & Context.all'Image);
   Context.Log.Trace ("Finalization of controlled object");
end Alice_Cmd;
