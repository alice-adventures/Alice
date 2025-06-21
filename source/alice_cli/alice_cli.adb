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
with Alice.IFace.OS_Cmd;
with Alice.OS_Context;
with Alice.Result;
with Alice.Std.Error_Handler;
with Alice.Std.Log;
with Alice.Std.Progress;
with Alice.Std.OS_Cmd;

with Test.Activity;

procedure Alice_CLI is

   OS_Ctx : constant Alice.OS_Context.Object :=
     (Err => new Alice.Std.Error_Handler.Object,
      Log => new Alice.Std.Log.Object);

   Ctx : constant Alice.Context.Object :=
     (Err      => OS_Ctx.Err,
      Log      => OS_Ctx.Log,
      Progress => new Alice.Std.Progress.Object,
      OS_Cmd   =>
        (Alr  => Alice.Std.OS_Cmd.New_Object ("alr"),
         Git  => Alice.Std.OS_Cmd.New_Object ("git"),
         Curl => Alice.Std.OS_Cmd.New_Object ("curl")));

   Result    : Alice.Result.Object'Class := Alice.Result.Null_Object;
   OS_Result : Alice.IFace.OS_Cmd.Exit_Result'Class :=
     Alice.IFace.OS_Cmd.Null_Exit_Result;

   procedure Test_Activity is
   begin
      Test.Activity.With_Success (Ctx, "Test number ONE ", 3);
      Ctx.Log.Info ("Changing activity");
      delay 2.0;
      Test.Activity.With_Success (Ctx, "Test number TWO ", 2);
      Ctx.Log.Info ("Changing activity");
      delay 2.0;
      Test.Activity.With_Exception (Ctx);

   exception
      when E : others =>
         Ctx.Log.Info ("Exception caught: " & Exception_Information (E));
   end Test_Activity;
   --  pragma Unreferenced (Test_Activity);

begin
   Result := Ctx.OS_Cmd.Alr.Initialize;
   Result := Ctx.OS_Cmd.Curl.Initialize;
   Result := Ctx.OS_Cmd.Git.Initialize;

   --  Ctx.Log.Optimize_For_CLI (With_Color_Enabled => False);
   Ctx.Log.Optimize_For_CLI (With_Color_Enabled => True);

   --  Ctx.Log.Set_Verbose_Level (False);
   --  Ctx.Log.Set_Verbose_Level (True);
   --  Ctx.Log.Set_Trace_Level (With_Location_Enabled => False);
   --  Ctx.Log.Set_Trace_Level (With_Location_Enabled => True);
   Ctx.Log.Set_Debug_Level (With_Location_Enabled => False);
   --  Ctx.Log.Set_Debug_Level (With_Location_Enabled => True);

   Ctx.Log.Trace_Begin;

   Result := Ctx.OS_Cmd.Alr.Initialize;
   Ctx.Log.Debug ("Initialize Alr command " & Result'Image);
   case Result.Status is
      when Alice.Result.Success =>
         OS_Result := Ctx.OS_Cmd.Alr.Run ("version", OS_Ctx);

      when Alice.Result.Error =>
         --  Ctx.Err.Handle_Error (Result);
         null;
   end case;

   --  Put_Line ("Welcome to the Alice " & Alice.Version & " CLI
   --  application!");

   Test_Activity;

   declare
      Query_Version : Alice.App.Query.Version.Use_Case := (Full_Text => False);
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

   declare
      Out_Result : Alice.IFace.OS_Cmd.Output_Result'Class :=
        Ctx.OS_Cmd.Git.Run ("remote -v", OS_Ctx);
      Result     : Alice.Result.Object'Class := Alice.Result.Null_Object;
   begin
      Alice.Std.OS_Cmd.Debug_Output_Result (Out_Result, OS_Ctx);
      Result := Ctx.OS_Cmd.Git.Cleanup (Out_Result, OS_Ctx);
   end;

   Ctx.Log.Trace_End;
end Alice_CLI;
