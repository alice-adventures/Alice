-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Alice;
with Alice.App.Query.Version;
with Alice.Context;
with Alice.Result;
with Alice.Std.Log;

--  with Test.Activity;

procedure Alice_CLI is

   --  Log : Alice.IFace.Logger.Object := Alice.Std.Log.Object;
   Ctx : constant Alice.Context.Object := (Log => new Alice.Std.Log.Object);

   --  procedure Test_Activity is
   --     Activity : Alice.Log.Activity.CLI.Object_CLI;
   --  begin
   --     Test.Activity.Success (Activity, "Test number ONE ", 3);
   --     Alice.Log.Info ("Changing activity");
   --     delay 2.0;
   --     Test.Activity.Success (Activity, "Test number TWO ", 2);
   --     Alice.Log.Info ("Changing activity");
   --     delay 2.0;
   --     Test.Activity.Fatal_Error (Activity);
   --  end Test_Activity;
   --  pragma Unreferenced (Test_Activity);

begin

   --  Ctx.Log.Optimize_For_CLI (With_Color_Enabled => False);
   Ctx.Log.Optimize_For_CLI (With_Color_Enabled => True);

   --  Ctx.Log.Set_Verbose_Level (False);
   --  Ctx.Log.Set_Verbose_Level (True);
   --  Ctx.Log.Set_Trace_Level (With_Location_Enabled => False);
   --  Ctx.Log.Set_Trace_Level (With_Location_Enabled => True);
   --  Ctx.Log.Set_Debug_Level (With_Location_Enabled => False);
   Ctx.Log.Set_Debug_Level (With_Location_Enabled => True);

   Ctx.Log.Trace_Begin;

   --  Put_Line ("Welcome to the Alice " & Alice.Version & " CLI
   --  application!");

   declare
      Query_Version : Alice.App.Query.Version.Use_Case;
      Result        : constant Alice.Result.Object'Class :=
        Query_Version.Run (Ctx);
   begin
      case Result.Status is
         when Alice.Result.Success =>
            declare
               R : constant Alice.App.Query.Version.Result :=
                 Alice.App.Query.Version.Result (Result);
            begin
               Put_Line ("Alice version: " & R.Version'Image);
            end;

         when Alice.Result.Error =>
            --  #FIXME - Handle error properly with an Error_Handler object
            Ctx.Log.Info
              ("Error retrieving Alice version: "
               & Alice.Str (Result.Message));
      end case;
   end;

   Ctx.Log.Trace_End;

exception
   when E : others =>
      Ctx.Log.Info ("Exception caught: " & Exception_Information (E));
end Alice_CLI;
