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

with Alice.Std.Error_Handler;
with Alice.Std.Log;
with Alice.Std.Progress;
with Alice.Std.OS_Cmd;

package body Alice.Std is

   Std_Err : constant Alice.IFace.Error_Handler.Object_Access :=
     new Alice.Std.Error_Handler.Object;

   Std_Log : constant Alice.IFace.Logger.Object_Access :=
     new Alice.Std.Log.Object;

   Std_Progress : constant Alice.IFace.Progress_Tracker.Object_Access :=
     new Alice.Std.Progress.Object;

   Std_OS_Context : constant Alice.OS_Context.Object_Access :=
     new Alice.OS_Context.Object'(Err => Std_Err, Log => Std_Log);

   Std_Alr_Cmd : constant Alice.IFace.OS_Cmd.Object_Access :=
     Alice.Std.OS_Cmd.New_Object ("alr", Std_OS_Context);

   Std_Curl_Cmd : constant Alice.IFace.OS_Cmd.Object_Access :=
     Alice.Std.OS_Cmd.New_Object ("curl", Std_OS_Context);

   Std_Git_Cmd : constant Alice.IFace.OS_Cmd.Object_Access :=
     Alice.Std.OS_Cmd.New_Object ("git", Std_OS_Context);

   Std_Context : constant Alice.Context.Object_Access :=
     new Alice.Context.Object'
       (Err      => Std_Err,
        Log      => Std_Log,
        Progress => Std_Progress,
        OS_Cmd   =>
          (Alr => Std_Alr_Cmd, Curl => Std_Curl_Cmd, Git => Std_Git_Cmd));

   --------------------
   -- Get_OS_Context --
   --------------------

   function Get_OS_Context return Alice.OS_Context.Object_Access
   is (Std_OS_Context);

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context return Alice.Context.Object_Access
   is (Std_Context);

end Alice.Std;
