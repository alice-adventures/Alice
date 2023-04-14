-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice_Config;

with CLIC.Subcommand.Instance;
with CLIC.TTY;
with GNAT.OS_Lib;
with Simple_Logging;

with Text_IO;

package body Alice_Command is

   package Log renames Simple_Logging;

   use all type Alice_Config.Build_Profile_Kind;

   type Global_Switches_Type (Profile : Alice_Config.Build_Profile_Kind) is
   record
      Help    : aliased Boolean := False;
      Color   : aliased Boolean := True;
      TTY     : aliased Boolean := True;
      Verbose : aliased Boolean := True;
      Detail  : aliased Boolean := False;
      Debug   : aliased Boolean := False;
   end record;

   Global_Switch : Global_Switches_Type (Alice_Config.Build_Profile);

   procedure Set_Global_Switches
     (Config : in out CLIC.Subcommand.Switches_Configuration);

   --!pp OFF
   package Command is new CLIC.Subcommand.Instance
     (Main_Command_Name   => Alice_Config.Crate_Name,
      Version             =>
        (if Alice_Config.Build_Profile /= Alice_Config.release then
           Alice_Config.Crate_Version & " (" &
           Alice_Config.Build_Profile'Image & ")"
         else Alice_Config.Crate_Version),
      Set_Global_Switches => Set_Global_Switches,
      Put                 => Text_IO.Put,
      Put_Line            => Text_IO.Put_Line,
      Put_Error           => Text_IO.Put_Line,
      Error_Exit          => GNAT.OS_Lib.OS_Exit,
      TTY_Chapter         => CLIC.TTY.Info,
      TTY_Description     => CLIC.TTY.Description,
      TTY_Version         => CLIC.TTY.Version,
      TTY_Underline       => CLIC.TTY.Underline,
      TTY_Emph            => CLIC.TTY.Emph);
   --!pp ON

   -------------------------
   -- Set_Global_Switches --
   -------------------------

   procedure Set_Global_Switches
     (Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;
   begin
      --!pp OFF
      Define_Switch (Config,
                     Global_Switch.Help'Access,
                     "-h", "--help",
                     "Display command help");
      Define_Switch (Config,
                     Global_Switch.Color'Access,
                     Long_Switch => "--no-color",
                     Help => "Disable color",
                     Value => False);
      Define_Switch (Config,
                     Global_Switch.TTY'Access,
                     Long_Switch => "--no-tty",
                     Help => "Disable control characters",
                     Value => False);
      Define_Switch (Config,
                     Global_Switch.Verbose'Access,
                     "-v", "--verbose",
                     Help => "Show command activity");

      pragma Warnings (Off);
      if Global_Switch.Profile = Alice_Config.development then
         Define_Switch (Config,
                        Global_Switch.Debug'Access,
                        "-g", "--debug",
                        Help => "Show debug information");
      end if;
      pragma Warnings (On);
      --!pp ON

   end Set_Global_Switches;

   -------------
   -- Execute --
   -------------

   procedure Execute is
   begin
      Command.Parse_Global_Switches;

      if Global_Switch.Debug then
         Log.Level := Log.Debug;
         Log.Debug ("Global_Switch" & Global_Switch'Image);
      end if;

      if not Global_Switch.TTY then
         CLIC.TTY.Force_Disable_TTY;
         Log.Debug ("Disable TTY");
      end if;

      if Global_Switch.Color and then Global_Switch.TTY then
         CLIC.TTY.Enable_Color (Force => False);
         Log.Debug ("Enable Color");
      end if;

      Log.Debug ("Command.Execute begin");
      Command.Execute;
      Log.Debug ("Command.Execute end");
   end Execute;

end Alice_Command;
