-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO;

with Alice;
with Alice.App.Query.Version;
with Alice.Context;
with Alice.IFace.OS_Cmd;
with Alice.OS_Context;
with Alice.Result;
with Alice.Std;
--  with Alice.Std.Error_Handler;
--  with Alice.Std.Log;
--  with Alice.Std.Progress;
with Alice.Std.OS_Cmd;

with Test.Logger;
with Test.Progress_Tracker;

procedure Alice_CLI is

   OS_Ctx : constant Alice.OS_Context.Object := Alice.Std.Get_OS_Context;
   Ctx    : constant Alice.Context.Object := Alice.Std.Get_Context;

   Result    : Alice.Result.Object'Class := Alice.Result.Null_Object;
   OS_Result : Alice.IFace.OS_Cmd.Exit_Result'Class :=
     Alice.IFace.OS_Cmd.Null_Exit_Result;

begin
   --  SELECT LOG LEVEL -------------------------------------------------------
   --  Ctx.Log.Optimize_For_CLI (With_Color_Enabled => False);
   Ctx.Log.Optimize_For_CLI (With_Color_Enabled => True);

   --  Ctx.Log.SetDefault_Level;
   Ctx.Log.Set_Verbose_Level;
   --  Ctx.Log.Set_Trace_Level (With_Location_Enabled => False);
   --  Ctx.Log.Set_Trace_Level (With_Location_Enabled => True);
   --  Ctx.Log.Set_Debug_Level (With_Location_Enabled => False);
   --  Ctx.Log.Set_Debug_Level (With_Location_Enabled => True);
   --  -------------------------------------------------------------------------

   Ctx.Log.Debug ("Initialize Alr command " & Result'Image);
   case Result.Status is
      when Alice.Result.Success =>
         OS_Result := Ctx.OS_Cmd.Alr.Run ("version", OS_Ctx);

      when Alice.Result.Error =>
         --  Ctx.Err.Handle_Error (Result);
         null;
   end case;

   Ada.Text_IO.Put_Line ("Welcome to the Alice CLI application!");

   Ctx.Log.Save_State;
   Test.Logger.Run (Ctx.Log);
   Ctx.Log.Restore_State;

   Test.Progress_Tracker.Run (Ctx.Log, Ctx.Progress);

   --  declare
   --     Query_Version : Alice.App.Query.Version.Use_Case := (Full_Text => False);
   --     Result        : constant Alice.Result.Object'Class :=
   --       Query_Version.Run (Ctx);
   --  begin
   --     case Result.Status is
   --        when Alice.Result.Success =>
   --           declare
   --              R : constant Alice.App.Query.Version.Result :=
   --                Alice.App.Query.Version.Result (Result);
   --           begin
   --              Put_Line ("Alice version: " & R.Version'Image);
   --           end;

   --        when Alice.Result.Error =>
   --           --  #FIXME - Handle error properly with an Error_Handler object
   --           Ctx.Log.Info
   --             ("Error retrieving Alice version: "
   --              & Alice.Str (Result.Message));
   --     end case;
   --  end;

   declare
      Out_Result : Alice.IFace.OS_Cmd.Output_Result'Class :=
        Ctx.OS_Cmd.Curl.Timed_Run
          ("https://ftp.funet.fi/pub/Linux/mirrors/ubuntu/releases/25.04/"
           & "ubuntu-25.04-netboot-amd64.tar.gz",
           OS_Ctx,
           1.0);
      Result     : constant Alice.Result.Object'Class :=
        Ctx.OS_Cmd.Curl.Cleanup (Out_Result, OS_Ctx);
   begin
      null;
   --  Alice.Std.OS_Cmd.Debug_Output_Result (Out_Result, OS_Ctx);
   --  Result := Ctx.OS_Cmd.Curl.Cleanup (Out_Result, OS_Ctx);
   end;

   Ctx.Log.Trace_End;
end Alice_CLI;
