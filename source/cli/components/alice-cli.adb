-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.UTF_Encoding;
with Ada.Text_IO;
--  with Alice.Result;
with GNAT.OS_Lib;

with CLIC.Subcommand.Instance;
with CLIC.TTY;

with Alice_Config;
with Alice.CLI.Profile;
with Alice.Context;
with Alice.Std;

package body Alice.CLI is

   use all type Alice_Config.Build_Profile_Kind;

   type Global_Switches_Type
     (Build_Profile : Alice_Config.Build_Profile_Kind)
   is record
      Help     : aliased Boolean := False;
      No_Color : aliased Boolean := False;
      No_TTY   : aliased Boolean := False;
      Verbose  : aliased Boolean := False; --  show command activity
      Version  : aliased Boolean := False; --  show version information
      Trace    : aliased Boolean := False; --  show trace information
      Debug    : aliased Boolean := False; --  show program details
   end record;

   Global_Switch : Global_Switches_Type (Alice_Config.Build_Profile);

   procedure Set_Global_Switches
     (Config : in out CLIC.Subcommand.Switches_Configuration);

   function TTY_Chapter
     (Text : Ada.Strings.UTF_Encoding.UTF_8_String := "")
      return Ada.Strings.UTF_Encoding.UTF_8_String
   is (CLIC.TTY.Bold (Text));
   --  Do no prepend Emph ("🛈") character to Text

   package CLI_Command is new
     CLIC.Subcommand.Instance
       (Main_Command_Name   => Alice_Config.Crate_Name,
        Version             =>
          (if Alice_Config.Build_Profile = Alice_Config.release
           then Alice_Config.Crate_Version
           else
             Alice_Config.Crate_Version
             & " ("
             & Alice_Config.Build_Profile'Image
             & ")"),
        Set_Global_Switches => Set_Global_Switches,
        Put                 => Ada.Text_IO.Put,
        Put_Line            => Ada.Text_IO.Put_Line,
        Put_Error           => Ada.Text_IO.Put_Line,
        Error_Exit          => GNAT.OS_Lib.OS_Exit,
        TTY_Chapter         => TTY_Chapter,
        TTY_Description     => CLIC.TTY.Description,
        TTY_Version         => CLIC.TTY.Version,
        TTY_Underline       => CLIC.TTY.Underline,
        TTY_Emph            => CLIC.TTY.Emph);

   -------------------------
   -- Set_Global_Switches --
   -------------------------

   procedure Set_Global_Switches
     (Config : in out CLIC.Subcommand.Switches_Configuration) is
   begin
      --!pp off
      CLIC.Subcommand.Define_Switch (
         Config,
         Global_Switch.Help'Access,
         "-h", "--help",
         "Display command help");

      CLIC.Subcommand.Define_Switch (
         Config,
         Global_Switch.No_Color'Access,
         Long_Switch => "--no-color",
         Help        => "Disable color");

      CLIC.Subcommand.Define_Switch (
         Config,
         Global_Switch.No_TTY'Access,
         Long_Switch => "--no-tty",
         Help        => "Disable control characters");

      CLIC.Subcommand.Define_Switch (
         Config,
         Global_Switch.Verbose'Access,
         Switch      => "-v",
         Long_Switch => "--verbose",
         Help        => "Show command activity");

      CLIC.Subcommand.Define_Switch (
         Config,
         Global_Switch.Version'Access,
         Switch      => "-V",
         Long_Switch => "--version",
         Help        => "Display version information");

      pragma Warnings (Off);
      if Alice_Config.Build_Profile = Alice_Config.development then
         CLIC.Subcommand.Define_Switch (
            Config,
            Global_Switch.Trace'Access,
            Switch      => "-T",
            Long_Switch => "--trace",
            Help        => "Show trace information");

         CLIC.Subcommand.Define_Switch (
            Config,
            Global_Switch.Debug'Access,
            Switch      => "-D",
            Long_Switch => "--debug",
            Help        => "Show debug information");
      end if;
      pragma Warnings (On);
      --!pp on
   end Set_Global_Switches;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      CLI_Command.Register ("General", new CLI_Command.Builtin_Help);
      CLI_Command.Register ("Configuration", new Alice.CLI.Profile.Object);
   end Initialize;

   -------------
   -- Execute --
   -------------

   procedure Execute is
      Context : constant Alice.Context.Object_Access := Alice.Std.Get_Context;
   begin
      CLI_Command.Parse_Global_Switches;

      if Global_Switch.No_TTY then
         CLIC.TTY.Force_Disable_TTY;
      end if;

      if not Global_Switch.No_Color and then not Global_Switch.No_TTY then
         CLIC.TTY.Enable_Color (Force => True);
         Context.Log.Optimize_For_CLI (With_Color_Enabled => True);
      else
         CLIC.TTY.Disable_Color;
         Context.Log.Optimize_For_CLI (With_Color_Enabled => False);
      end if;

      Context.Log.Initialize;
      Context.Log.Set_Default_Level;

      if Global_Switch.Verbose then
         Context.Log.Set_Verbose_Level;
      end if;

      pragma Warnings (Off);
      if Alice_Config.Build_Profile = Alice_Config.development then
         if Global_Switch.Trace then
            Context.Log.Set_Trace_Level (With_Location_Enabled => True);
            Context.Log.Info ("Trace level enabled");
         elsif Global_Switch.Debug then
            Context.Log.Set_Debug_Level (With_Location_Enabled => True);
            Context.Log.Info ("Debug level enabled");
         end if;
      end if;
      pragma Warnings (On);

      Context.Log.Debug (Global_Switch'Image);

      if Global_Switch.Version then
         Ada.Text_IO.Put_Line ("Version: " & Alice_Config.Crate_Version);
         Ada.Text_IO.Put_Line
           ("Build Profile: " & Alice_Config.Build_Profile'Image);
         Ada.Text_IO.Put_Line ("Build Host: " & Alice_Config.Alire_Host_OS);
         GNAT.OS_Lib.OS_Exit (0);
      end if;

      CLI_Command.Execute;
   end Execute;

end Alice.CLI;
