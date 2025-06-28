-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GNAT.Source_Info;

package body Test.Logger is

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

   ----------------------------------
   -- Optimized_For_CLI_With_Color --
   ----------------------------------

   procedure Optimized_For_CLI_With_Color
     (Log : Alice.IFace.Logger.Object_Access) is
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);

      Log.Optimize_For_CLI (With_Color_Enabled => True);
      Test_All_Levels (Log);
   end Optimized_For_CLI_With_Color;

   -------------------------------------
   -- Optimized_For_CLI_Without_Color --
   -------------------------------------

   procedure Optimized_For_CLI_Without_Color
     (Log : Alice.IFace.Logger.Object_Access) is
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);

      Log.Optimize_For_CLI (With_Color_Enabled => False);
      Test_All_Levels (Log);
   end Optimized_For_CLI_Without_Color;

   ---------
   -- Run --
   ---------

   procedure Run (Log : Alice.IFace.Logger.Object_Access) is
   begin
      Optimized_For_CLI_With_Color (Log);
      Optimized_For_CLI_Without_Color (Log);
   end Run;

end Test.Logger;
