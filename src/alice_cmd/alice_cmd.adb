-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice_Config;

with Alice_Cmd.Setup.Check;
with Alice_Cmd.PSource.List;
with Alice_Cmd.PSource.Init;

with CLIC.Subcommand.Instance;
with CLIC.TTY;
with GNAT.OS_Lib;
with Simple_Logging;

with Text_IO;

package body Alice_Cmd is

   package Log renames Simple_Logging;

   use all type Alice_Config.Build_Profile_Kind;

   type Global_Switches_Type (Profile : Alice_Config.Build_Profile_Kind) is
   record
      Help    : aliased Boolean := False;
      Color   : aliased Boolean := True;
      TTY     : aliased Boolean := True;
      Verbose : aliased Boolean := False; --  describe command activity
      Detail  : aliased Boolean := False; --  show command details
      Debug   : aliased Boolean := False; --  show program details
   end record;

   Global_Switch : Global_Switches_Type (Alice_Config.Build_Profile);

   procedure Set_Global_Switches
     (Config : in out CLIC.Subcommand.Switches_Configuration);

   --!pp OFF
   package CLI_Command is new CLIC.Subcommand.Instance
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
      Define_Switch (Config,
                     Global_Switch.Detail'Access,
                     "-d", "--detail",
                     Help => "Show command details");

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
      CLI_Command.Parse_Global_Switches;

      Log.Level := Log.Warning;

      if Global_Switch.Verbose then
         Log.Level := Log.Info;
         Log.Info ("describe command activity");
      end if;

      if Global_Switch.Detail then
         Log.Level := Log.Detail;
         Log.Detail ("show command details");
      end if;

      if Global_Switch.Debug then
         Log.Level := Log.Debug;
         Log.Debug ("show program details");
         Log.Debug ("Global_Switch" & Global_Switch'Image);
      end if;

      if not Global_Switch.TTY then
         CLIC.TTY.Force_Disable_TTY;
         Log.Detail ("disable TTY");
      end if;

      if Global_Switch.Color and then Global_Switch.TTY then
         CLIC.TTY.Enable_Color (Force => False);
         Log.Detail ("enable Color");
      end if;

      Log.Debug ("Command.Execute begin");
      CLI_Command.Execute;
      Log.Debug ("Command.Execute end");
   end Execute;

begin

   CLI_Command.Register ("General", new CLI_Command.Builtin_Help);

   CLI_Command.Register
     ("Setup", new Alice_Cmd.Setup.Check.Cmd_Type);

   CLI_Command.Register
     ("Problem Sources", new Alice_Cmd.PSource.List.Cmd_Type);
   CLI_Command.Register
     ("Problem Sources", new Alice_Cmd.PSource.Init.Cmd_Type);

end Alice_Cmd;
