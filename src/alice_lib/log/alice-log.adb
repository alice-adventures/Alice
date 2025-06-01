-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GNAT.IO;
with GNAT.OS_Lib;

with AnsiAda;
with Simple_Logging;
with Simple_Logging.Decorators;

with Alice_Config;

package body Alice.Log is

   package ANSI renames AnsiAda;

   use all type ANSI.Colors;
   use all type ANSI.Styles;
   use all type Alice_Config.Build_Profile_Kind;

   Is_Optimized_For_GUI : Boolean := False;
   --  This is set to True when the logging is optimized for GUI applications.
   --  It is used to determine whether to redirect the output to the standard
   --  error or the standard output.

   pragma Unreferenced (Is_Optimized_For_GUI);
   --  #FIXME - Remove this pragma once the GUI spinner is implemented.

   --  #TODO - Add a variable to access the instance of the busy status
   --  spinner used in GUI applications. More or less like this:
   --
   --  GUI_Spinner : Alice.GUI.Spinner'Access := null;

   ------------------------------
   -- Regular_Level_Decorators --
   ------------------------------

   function Regular_Level_Decorators
     (Level : Simple_Logging.Levels; Message : String) return String is
   begin
      return
        (case Level is
           when Simple_Logging.Error => "ERROR: " & Message,
           when Simple_Logging.Warning => "Warning: " & Message,
           when Simple_Logging.Info => "info: " & Message,
           when Simple_Logging.Detail => "Trace: " & Message,
           when Simple_Logging.Debug => "DEBUG: " & Message,
           when others => Message);
   end Regular_Level_Decorators;

   ----------------------------
   -- Color_Level_Decorators --
   ----------------------------

   function Color_Level_Decorators
     (Level : Simple_Logging.Levels; Message : String) return String is
   begin
      return
        (case Level is
           when Simple_Logging.Error =>
             ANSI.Wrap
               (Text       => "ERROR:",
                Style      => Bright,
                Foreground => ANSI.Foreground (Red))
             & " "
             & Message,
           when Simple_Logging.Warning =>
             ANSI.Wrap
               (Text       => "Warning:",
                Style      => Bright,
                Foreground => ANSI.Foreground (Yellow))
             & " "
             & Message,
           when Simple_Logging.Info =>
             ANSI.Wrap
               (Text       => "info:",
                Style      => Bright,
                Foreground => ANSI.Foreground (Green))
             & " "
             & Message,
           when Simple_Logging.Detail =>
             ANSI.Wrap
               (Text       => "Trace:",
                Style      => Bright,
                Foreground => ANSI.Foreground (Cyan))
             & " "
             & Message,
           when Simple_Logging.Debug =>
             ANSI.Wrap
               (Text       => "DEBUG:",
                Style      => Default,
                Foreground => ANSI.Foreground (Grey))
             & " "
             & ANSI.Wrap (Text => Message, Style => Dim),
           when others => Message);
   end Color_Level_Decorators;

   -----------------------------
   -- Enable_Color_Decorators --
   -----------------------------
   procedure Enable_Color_Decorators (Yes : Boolean := True) is
   begin
      Simple_Logging.Decorators.Level_Decorator :=
        (if Yes
         then Color_Level_Decorators'Access
         else Regular_Level_Decorators'Access);
   end Enable_Color_Decorators;

   -------------------------------
   -- Enable_Location_Decorator --
   -------------------------------

   procedure Enable_Location_Decorator (Yes : Boolean := True) is
   begin
      Simple_Logging.Decorators.Location_Decorator :=
        (if Yes
         then Simple_Logging.Decorators.Simple_Location_Decorator'Access
         else Simple_Logging.Decorators.No_Location_Decorator'Access);
   end Enable_Location_Decorator;

   ----------------------
   -- Optimize_For_CLI --
   ----------------------

   procedure Optimize_For_CLI (With_Color_Enabled : Boolean := True) is
   begin
      GNAT.IO.Set_Output (GNAT.IO.Standard_Output);
      Enable_Color_Decorators (With_Color_Enabled);
      Simple_Logging.Level := Simple_Logging.Warning;
      Simple_Logging.Is_TTY := True;
      Simple_Logging.ASCII_Only := False;
   end Optimize_For_CLI;

   ----------------------
   -- Optimize_For_GUI --
   ----------------------

   procedure Optimize_For_GUI --  #TODO - Add an abstract Spinner parameter
   is
   begin
      Is_Optimized_For_GUI := True;
      GNAT.IO.Set_Output (GNAT.IO.Standard_Error);
      Enable_Color_Decorators (False);
      Simple_Logging.Is_TTY := False;
      Simple_Logging.ASCII_Only := True;
   end Optimize_For_GUI;

   -----------------
   -- Set_Verbose --
   -----------------

   procedure Set_Verbose_Level (Verbose : Boolean) is
   begin
      Simple_Logging.Level :=
        (if Verbose then Simple_Logging.Info else Simple_Logging.Warning);
   end Set_Verbose_Level;

   ---------------------
   -- Set_Trace_Level --
   ---------------------

   procedure Set_Trace_Level (With_Location_Enabled : Boolean := True) is
   begin
      case Alice_Config.Build_Profile is

         when Alice_Config.release =>
            Enable_Location_Decorator (False);

         when others =>
            Simple_Logging.Level := Simple_Logging.Detail;
            Enable_Location_Decorator (With_Location_Enabled);

      end case;
   end Set_Trace_Level;

   ---------------------
   -- Set_Debug_Level --
   ---------------------

   procedure Set_Debug_Level (With_Location_Enabled : Boolean := True) is
   begin
      case Alice_Config.Build_Profile is

         when Alice_Config.release =>
            Enable_Location_Decorator (False);

         when others =>
            Simple_Logging.Level := Simple_Logging.Debug;
            Enable_Location_Decorator (With_Location_Enabled);

      end case;
   end Set_Debug_Level;

   -----------
   -- Error --
   -----------

   procedure Error
     (Msg       : String;
      Exit_Code : Integer := 0;
      Entity    : String := Enclosing_Entity;
      Location  : String := Source_Location) is
   begin
      Simple_Logging.Error (Msg, Entity, Location);
      if Exit_Code /= 0 then
         GNAT.OS_Lib.OS_Exit (Exit_Code);
      end if;
   end Error;

   -------------
   -- Warning --
   -------------

   procedure Warning
     (Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location) renames Simple_Logging.Warning;

   ----------
   -- Info --
   ----------

   procedure Info
     (Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location) renames Simple_Logging.Info;

   -----------------
   -- Trace_Begin --
   -----------------

   procedure Trace_Begin
     (Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location)
   is separate;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location)
   is separate;

   ---------------
   -- Trace_End --
   ---------------

   procedure Trace_End
     (Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location)
   is separate;

   -----------
   -- Debug --
   -----------

   procedure Debug
     (Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location)
   is separate;

end Alice.Log;
