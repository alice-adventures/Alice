-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Exceptions;
with GNAT.Source_Info;

with Simple_Logging.Decorators;

package body Test.Log is

   --------------------
   -- Log_All_Levels --
   --------------------

   procedure Log_All_Levels (Log : Alice.IFace.Logger.Object_Access) is
   begin
      Log.Info ("This is an Info message");
      Log.Warning ("This is a Warning message");
      Log.Trace_Begin ("This is a Trace_Begin message");
      Log.Trace ("This is a Trace message");
      Log.Trace_Return ("This is a Trace_Return message");
      Log.Trace_End ("This is a Trace_End message");
      Log.Debug ("This is a Debug message");

      Test.Pass;

   exception
      when E : others =>
         Test.Fail
           ("Exception caught: " & Ada.Exceptions.Exception_Information (E));
   end Log_All_Levels;

   ---------------------
   -- Test_All_Levels --
   ---------------------

   procedure Test_All_Levels (Log : Alice.IFace.Logger.Object_Access) is
   begin
      Test.Subtitle ("Default level");
      Log.Set_Default_Level;
      Log_All_Levels (Log);

      Test.Subtitle ("Verbose level");
      Log.Set_Verbose_Level;
      Log_All_Levels (Log);

      Test.Subtitle ("Trace level - location disabled");
      Log.Set_Trace_Level (With_Location_Enabled => False);
      Log_All_Levels (Log);

      Test.Subtitle ("Trace level - location enabled");
      Log.Set_Trace_Level (With_Location_Enabled => True);
      Log_All_Levels (Log);

      Test.Subtitle ("Debug level - location disabled");
      Log.Set_Debug_Level (With_Location_Enabled => False);
      Log_All_Levels (Log);

      Test.Subtitle ("Debug level - location enabled");
      Log.Set_Debug_Level (With_Location_Enabled => True);
      Log_All_Levels (Log);
   end Test_All_Levels;

   package Optimize_For_CLI is

      procedure With_Color (Log : Alice.IFace.Logger.Object_Access);

      procedure Without_Color (Log : Alice.IFace.Logger.Object_Access);

   end Optimize_For_CLI;

   package body Optimize_For_CLI is

      ----------------
      -- With_Color --
      ----------------

      procedure With_Color (Log : Alice.IFace.Logger.Object_Access) is
      begin
         Test.Title (GNAT.Source_Info.Enclosing_Entity);

         Log.Optimize_For_CLI (With_Color_Enabled => True);
         Test_All_Levels (Log);
      end With_Color;

      -------------------
      -- Without_Color --
      -------------------

      procedure Without_Color (Log : Alice.IFace.Logger.Object_Access) is
      begin
         Test.Title (GNAT.Source_Info.Enclosing_Entity);

         Log.Optimize_For_CLI (With_Color_Enabled => False);
         Test_All_Levels (Log);
      end Without_Color;

   end Optimize_For_CLI;

   -------------------
   -- Run_All_Tests --
   -------------------

   procedure Run_All_Tests (Log : Alice.IFace.Logger.Object_Access) is
   begin
      Optimize_For_CLI.Without_Color (Log);
      Optimize_For_CLI.With_Color (Log);
   end Run_All_Tests;

end Test.Log;
