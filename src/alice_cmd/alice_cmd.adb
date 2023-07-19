-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice_Env;
with Alice_Config;

with Alice_Cmd.Setup.Check;
with Alice_Cmd.Setup.Config;
with Alice_Cmd.Work.Source;

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
      Quiet   : aliased Boolean := False; --  limit output
      Verbose : aliased Boolean := False; --  show command activity
      Debug   : aliased Boolean := False; --  show program details
   end record;

   Global_Switch : Global_Switches_Type (Alice_Config.Build_Profile);

   procedure Set_Global_Switches
     (Config : in out CLIC.Subcommand.Switches_Configuration);

   --!pp off
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
   --!pp on

   -------------------------
   -- Set_Global_Switches --
   -------------------------

   procedure Set_Global_Switches
     (Config : in out CLIC.Subcommand.Switches_Configuration)
   is
      use CLIC.Subcommand;
   begin
      --!pp off
      pragma Style_Checks (off);

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
                     Global_Switch.Quiet'Access,
                     "-q", "--quiet",
                     Help => "Limit output to strictly necessary");
      Define_Switch (Config,
                     Global_Switch.Verbose'Access,
                     "-v", "--verbose",
                     Help => "Show command activity");

      pragma Warnings (Off);
      if Global_Switch.Profile = Alice_Config.development then
         Define_Switch (Config,
                        Global_Switch.Debug'Access,
                        "-d", "--debug",
                        Help => "Show debug information");
      end if;
      pragma Warnings (On);

      pragma Style_Checks (on);
      --!pp on

   end Set_Global_Switches;

   ---------------------
   -- Abort_Execution --
   ---------------------

   procedure Abort_Execution (Error : String; Exit_Status : Integer := 1) is
   begin
      Log.Error (Error);
      GNAT.OS_Lib.OS_Exit (Exit_Status);
   end Abort_Execution;

   -------------
   -- Execute --
   -------------

   procedure Execute is
   begin
      CLI_Command.Parse_Global_Switches;

      --  default log level
      Log.Level := Log.Info;

      if Global_Switch.Quiet then
         Log.Level := Log.Warning;
      end if;

      if Global_Switch.Verbose then
         Log.Level := Log.Detail;
         Log.Info ("show command activity");
      end if;

      if Global_Switch.Debug then
         Log.Level := Log.Debug;
         Log.Debug ("show debug information");
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

      if Alice_Env.Is_Alice_Root_Dir then
         Log.Detail ("begin Command.Execute");
         CLI_Command.Execute;
         Log.Detail ("end Command.Execute");
      end if;
   end Execute;

   -----------------------------
   -- Check_Unique_Subcommand --
   -----------------------------

   procedure Check_Unique_Subcommand (Number : Positive) is
   begin
      if Number > 1 then
         Abort_Execution ("Only one subcommand can be specified");
      end if;
   end Check_Unique_Subcommand;

   ---------------------------
   -- Check_Argument_Length --
   ---------------------------

   procedure Check_Argument_Length (Number, Length : Positive) is
   begin
      if Number < Length then
         Abort_Execution ("Too few arguments provided");
      end if;
      if Number > Length then
         Abort_Execution ("Too many argument provided");
      end if;
   end Check_Argument_Length;

   --------------------------
   -- Subcommand_Not_Found --
   --------------------------

   procedure Subcommand_Not_Found is
   begin
      Abort_Execution ("System error, subcommand not found");
   end Subcommand_Not_Found;

begin

   CLI_Command.Register ("General", new CLI_Command.Builtin_Help);

   CLI_Command.Register ("Setup", new Alice_Cmd.Setup.Check.Cmd_Type);
   CLI_Command.Register ("Setup", new Alice_Cmd.Setup.Config.Cmd_Type);

   CLI_Command.Register ("Contents", new Alice_Cmd.Work.Source.Cmd_Type);

end Alice_Cmd;
