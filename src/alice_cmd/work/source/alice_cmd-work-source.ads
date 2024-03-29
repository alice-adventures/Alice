-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with AAA.Strings;
with CLIC.Subcommand;

package Alice_Cmd.Work.Source is

   Command_Source_Error : exception;

   type Cmd_Type is new CLIC.Subcommand.Command with private;

   overriding function Name
     (Cmd : Cmd_Type) return CLIC.Subcommand.Identifier is
     ("source");

   overriding function Usage_Custom_Parameters
     (Cmd : Cmd_Type) return String is
     ("[ --list ] | { --init | --status | --update } <source>");

   overriding function Short_Description (Cmd : Cmd_Type) return String is
     ("Manage Problem Sources");

   --!pp off
   pragma Style_Checks (off);

   overriding function Long_Description
     (Cmd : Cmd_Type) return AAA.Strings.Vector is
     (AAA.Strings.Empty_Vector
         .Append ("Manage Problem Sources, including the setup from scratch, show the overall status and update the libraries and tools.")
         .New_Line
         .Append ("With no option, show the list of available Problem Sources, same as '--list'.")
     );

   pragma Style_Checks (on);
   --!pp on

   overriding function Switch_Parsing
     (Cmd : Cmd_Type) return CLIC.Subcommand.Switch_Parsing_Kind is
     (CLIC.Subcommand.Parse_All);

   overriding procedure Setup_Switches
     (Cmd    : in out Cmd_Type;
      Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding procedure Execute
     (Cmd : in out Cmd_Type; Args : AAA.Strings.Vector);

private

   type Cmd_Type is new CLIC.Subcommand.Command with record
      List   : aliased Boolean := False;
      Init   : aliased Boolean := False;
      Status : aliased Boolean := False;
      Update : aliased Boolean := False;
   end record;

end Alice_Cmd.Work.Source;
