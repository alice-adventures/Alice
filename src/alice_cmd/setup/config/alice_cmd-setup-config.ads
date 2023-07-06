-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with AAA.Strings;
with CLIC.Subcommand;

package Alice_Cmd.Setup.Config is

   type Cmd_Type is new CLIC.Subcommand.Command with null record;

   overriding function Name
     (Cmd : Cmd_Type) return CLIC.Subcommand.Identifier is
     ("config");

   overriding function Usage_Custom_Parameters
     (Cmd : Cmd_Type) return String is
     ("");

   overriding function Short_Description (Cmd : Cmd_Type) return String is
     ("Config Participant profile");

   --!pp off
   pragma Style_Checks (off);

   overriding function Long_Description
     (Cmd : Cmd_Type) return AAA.Strings.Vector is
     (AAA.Strings.Empty_Vector
         .Append ("Config Participant profile. This include GitHub username, email address and GitHub personal access token to enable interaction through the GitHub REST API.")
         .Append ("Visit https://github.com/settings/tokens to create your own token to work with Alice. It must have the 'repo' scope. Select the expiration date of your choice. Once expired, generate a new one and use this command to update it.")
     );

   pragma Style_Checks (on);
   --!pp on

   overriding function Switch_Parsing
     (Cmd : Cmd_Type) return CLIC.Subcommand.Switch_Parsing_Kind is
     (CLIC.Subcommand.Parse_All);

   overriding procedure Setup_Switches
     (Cmd    : in out Cmd_Type;
      Config : in out CLIC.Subcommand.Switches_Configuration) is null;

   overriding procedure Execute
     (Cmd : in out Cmd_Type; Args : AAA.Strings.Vector);

end Alice_Cmd.Setup.Config;
