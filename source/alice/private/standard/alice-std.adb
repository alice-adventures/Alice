-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.IFace.Error_Handler;
with Alice.IFace.Logger;
with Alice.IFace.Progress_Tracker;
with Alice.IFace.OS_Cmd;

with Alice.Result;

with Alice.Std.Error_Handler;
with Alice.Std.Log;
with Alice.Std.Progress;
with Alice.Std.OS_Cmd;

package body Alice.Std is

   use all type Alice.Result.Status_Type;

   Std_Err : constant Alice.IFace.Error_Handler.Object_Access :=
     new Alice.Std.Error_Handler.Object;

   Std_Log : constant Alice.IFace.Logger.Object_Access :=
     new Alice.Std.Log.Object;

   Std_Progress : constant Alice.IFace.Progress_Tracker.Object_Access :=
     new Alice.Std.Progress.Object;

   Std_OS_Ctx : constant Alice.OS_Context.Object_Access :=
     new Alice.OS_Context.Object'(Err => Std_Err, Log => Std_Log);

   Std_Alr_Cmd : constant Alice.IFace.OS_Cmd.Object_Access :=
     Alice.Std.OS_Cmd.New_Object ("alr", Std_OS_Ctx);

   Std_Curl_Cmd : constant Alice.IFace.OS_Cmd.Object_Access :=
     Alice.Std.OS_Cmd.New_Object ("curl", Std_OS_Ctx);

   Std_Git_Cmd : constant Alice.IFace.OS_Cmd.Object_Access :=
     Alice.Std.OS_Cmd.New_Object ("git", Std_OS_Ctx);

   Std_Ctx : constant Alice.Context.Object_Access :=
     new Alice.Context.Object'
       (Err    => Std_Err,
        Log    => Std_Log,
        Prog   => Std_Progress,
        OS_Cmd =>
          (Alr => Std_Alr_Cmd, Curl => Std_Curl_Cmd, Git => Std_Git_Cmd));

   Std_Ctx_Initialized : Boolean := False;

   --------------------
   -- Get_OS_Context --
   --------------------

   function Get_OS_Context return Alice.OS_Context.Object_Access
   is (Std_OS_Ctx);

   -----------------
   -- Init_OS_Cmd --
   -----------------

   procedure Init_OS_Cmd (Cmd : Alice.IFace.OS_Cmd.Object_Access) is
   begin
      if not Cmd.Is_Valid then
         Cmd.Initialize;
      end if;

   exception
      when Program_Error =>
         Result : constant Alice.Result.Error_Object :=
           (Status  => Alice.Result.Error,
            Level   => Alice.Result.System,
            Message =>
              Alice.UStr ("Failed to initialize OS command: " & Cmd.Name));

         Std_Err.Exit_Application
           (Result,
            Alice.UStr
              ("Make sure the command "
               & Cmd.Name
               & " is installed "
               & "and available in your PATH."));
   end Init_OS_Cmd;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context return Alice.Context.Object_Access is
   begin
      if not Std_Ctx_Initialized then
         Init_OS_Cmd (Std_Ctx.OS_Cmd.Alr);
         Init_OS_Cmd (Std_Ctx.OS_Cmd.Curl);
         Init_OS_Cmd (Std_Ctx.OS_Cmd.Git);
         Std_Ctx_Initialized := True;
      end if;
      return Std_Ctx;
   end Get_Context;

end Alice.Std;
