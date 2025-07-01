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

   Std_Alr_Cmd : constant Alice.IFace.OS_Cmd.Object_Access :=
     Alice.Std.OS_Cmd.New_Object ("alr");

   Std_Curl_Cmd : constant Alice.IFace.OS_Cmd.Object_Access :=
     Alice.Std.OS_Cmd.New_Object ("curl");

   Std_Git_Cmd : constant Alice.IFace.OS_Cmd.Object_Access :=
     Alice.Std.OS_Cmd.New_Object ("git");

   --------------------
   -- Get_OS_Context --
   --------------------

   function Get_OS_Context return Alice.OS_Context.Object is
   begin
      return
         OS_Ctx : constant Alice.OS_Context.Object :=
           (Err => Std_Err, Log => Std_Log)
      do
         null;
      end return;
   end Get_OS_Context;

   -----------------
   -- Init_OS_Cmd --
   -----------------

   procedure Init_OS_Cmd (Cmd : Alice.IFace.OS_Cmd.Object_Access) is
   begin
      if not Cmd.Is_Valid then
         Result : constant Alice.Result.Object'Class := Cmd.Initialize;

         if Result.Status = Alice.Result.Error then
            Std_Err.Exit_Application
              (Result,
               Alice.UStr
                 ("Make sure the command "
                  & Cmd.Name
                  & " is installed "
                  & "and available in your PATH."));
         end if;
      end if;
   end Init_OS_Cmd;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context return Alice.Context.Object is
   begin
      return
         Ctx : constant Alice.Context.Object :=
           (Err      => Std_Err,
            Log      => Std_Log,
            Progress => Std_Progress,
            OS_Cmd   =>
              (Alr => Std_Alr_Cmd, Curl => Std_Curl_Cmd, Git => Std_Git_Cmd))
      do
         Init_OS_Cmd (Ctx.OS_Cmd.Alr);
         Init_OS_Cmd (Ctx.OS_Cmd.Curl);
         Init_OS_Cmd (Ctx.OS_Cmd.Git);
      end return;
   end Get_Context;

end Alice.Std;
