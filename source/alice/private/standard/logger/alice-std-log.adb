-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GNAT.IO;

with AnsiAda;
with Simple_Logging;
with Simple_Logging.Decorators;

with Alice_Config;
with Alice.Env;

package body Alice.Std.Log is

   package ANSI renames AnsiAda;

   use all type ANSI.Colors;
   use all type ANSI.Styles;
   use all type Alice_Config.Build_Profile_Kind;

   Is_Optimized_For_GUI : Boolean := False;
   --  This is set to True when the logging is optimized for GUI applications.
   --  It is used to determine whether to redirect the output to the standard
   --  error or the standard output.

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

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (Self : in out Object) is
   begin
      case Alice_Config.Build_Profile is
         when Alice_Config.release =>
            Self.Set_Default_Level;

         when others =>
            Self.Set_Debug_Level (True);
      end case;
   end Initialize;

   ----------------------
   -- Optimize_For_CLI --
   ----------------------

   overriding
   procedure Optimize_For_CLI
     (Self : in out Object; With_Color_Enabled : Boolean := True) is
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

   overriding
   procedure Optimize_For_GUI
     (Self : in out Object) --  #TODO - Add an abstract Spinner parameter
   is
   begin
      Is_Optimized_For_GUI := True;
      GNAT.IO.Set_Output (GNAT.IO.Standard_Error);
      Enable_Color_Decorators (False);
      Simple_Logging.Is_TTY := False;
      Simple_Logging.ASCII_Only := True;
   end Optimize_For_GUI;

   -----------------------
   -- Set_Default_Level --
   -----------------------

   overriding
   procedure Set_Default_Level (Self : in out Object) is
   begin
      Simple_Logging.Level := Simple_Logging.Warning;
      Enable_Location_Decorator (False);
      Enable_Color_Decorators (True);
   end Set_Default_Level;

   -----------------
   -- Set_Verbose --
   -----------------

   overriding
   procedure Set_Verbose_Level (Self : in out Object) is
   begin
      Simple_Logging.Level := Simple_Logging.Info;
      Enable_Location_Decorator (False);
   end Set_Verbose_Level;

   ---------------------
   -- Set_Trace_Level --
   ---------------------

   overriding
   procedure Set_Trace_Level
     (Self : in out Object; With_Location_Enabled : Boolean := True) is
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

   overriding
   procedure Set_Debug_Level
     (Self : in out Object; With_Location_Enabled : Boolean := True) is
   begin
      case Alice_Config.Build_Profile is
         when Alice_Config.release =>
            Enable_Location_Decorator (False);

         when others =>
            Simple_Logging.Level := Simple_Logging.Debug;
            Enable_Location_Decorator (With_Location_Enabled);
      end case;
   end Set_Debug_Level;

   ----------
   -- Info --
   ----------

   overriding
   procedure Info
     (Self     : in out Object;
      Message  : String;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity;
      Location : String := GNAT.Source_Info.Source_Location) is
   begin
      Simple_Logging.Info (Message, Entity, Location);
   end Info;

   -------------
   -- Warning --
   -------------

   overriding
   procedure Warning
     (Self     : in out Object;
      Message  : String;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity;
      Location : String := GNAT.Source_Info.Source_Location) is
   begin
      Simple_Logging.Warning (Message, Entity, Location);
   end Warning;

   -----------------
   -- Trace_Begin --
   -----------------

   overriding
   procedure Trace_Begin
     (Self     : in out Object;
      Message  : String := "";
      Entity   : String := GNAT.Source_Info.Enclosing_Entity;
      Location : String := GNAT.Source_Info.Source_Location) is separate;

   -----------
   -- Trace --
   -----------

   overriding
   procedure Trace
     (Self     : in out Object;
      Message  : String;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity;
      Location : String := GNAT.Source_Info.Source_Location) is separate;

   ------------------
   -- Trace_Return --
   ------------------

   overriding
   procedure Trace_Return
     (Self     : in out Object;
      Message  : String := "";
      Entity   : String := GNAT.Source_Info.Enclosing_Entity;
      Location : String := GNAT.Source_Info.Source_Location) is separate;

   ---------------
   -- Trace_End --
   ---------------

   overriding
   procedure Trace_End
     (Self     : in out Object;
      Message  : String := "";
      Entity   : String := GNAT.Source_Info.Enclosing_Entity;
      Location : String := GNAT.Source_Info.Source_Location) is separate;

   -----------
   -- Debug --
   -----------

   overriding
   procedure Debug
     (Self     : in out Object;
      Message  : String;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity;
      Location : String := GNAT.Source_Info.Source_Location) is separate;

   --  Private, package-local saved state

   Saved_Level              : Simple_Logging.Levels;
   Saved_Level_Decorator    :
     access function
       (Level : Simple_Logging.Levels; Message : String) return String;
   Saved_Location_Decorator :
     access function (Entity, Location, Message : String) return String;

   ----------------
   -- Save_State --
   ----------------

   overriding
   procedure Save_State (Self : in out Object) is
   begin
      Saved_Level := Simple_Logging.Level;
      Saved_Level_Decorator := Simple_Logging.Decorators.Level_Decorator;
      Saved_Location_Decorator := Simple_Logging.Decorators.Location_Decorator;
   end Save_State;

   -------------------
   -- Restore_State --
   -------------------

   overriding
   procedure Restore_State (Self : in out Object) is
   begin
      Simple_Logging.Level := Saved_Level;
      Simple_Logging.Decorators.Level_Decorator := Saved_Level_Decorator;
      Simple_Logging.Decorators.Location_Decorator := Saved_Location_Decorator;
   end Restore_State;

   ------------------
   -- Logger_Image --
   ------------------

   procedure Logger_Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Self   : Object) is
   begin
      Output.Put ("([" & Self'Address'Image & " ] with");
      Alice.Env.Increase_Indent (Output);
      Output.New_Line;

      Output.Put ("Level                => " & Simple_Logging.Level'Image);
      Output.New_Line;
      Output.Put ("Is_Optimized_For_GUI => " & Is_Optimized_For_GUI'Image);

      Output.New_Line;
      Alice.Env.Decrease_Indent (Output);
      Output.Put (")");
   end Logger_Put_Image;

end Alice.Std.Log;
