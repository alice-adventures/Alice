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

   type Cmd_Type is new CLIC.Subcommand.Command with private;

   overriding function Name
     (Cmd : Cmd_Type) return CLIC.Subcommand.Identifier is
     ("config");

   overriding function Usage_Custom_Parameters
     (Cmd : Cmd_Type) return String is
     ("[ --show ] | --refresh | --token <github_token> | --license <spdx_id>");

   overriding function Short_Description (Cmd : Cmd_Type) return String is
     ("Configure Participant profile");

   --!pp off
   pragma Style_Checks (off);

   overriding function Long_Description
     (Cmd : Cmd_Type) return AAA.Strings.Vector is
     (AAA.Strings.Empty_Vector
         .Append ("Configure Participant profile. This includes the name, the GitHub login and the email address. This information is taken from GitHub (or 'git config') using a personal access token associated to a valid GitHub personal account (of a user, not an organization).")
         .New_Line
         .Append ("The GitHub token enables interaction through the GitHub REST API. It is necessary to clone Problem Sources repositories in your GitHub account. All repositories created to work with Alice share the prefix 'alice-', e.g. 'alice-project_euler'.")
         .New_Line
         .Append ("Your GitHub token exclusively created to work with Alice must be kept in secret. Do not shared it with other users and do not use it for other applications. It is stored in the user configuration file, which is ignored by git to not to push it accidentally (GitHub automatically revokes all pushed tokens).")
         .New_Line
         .Append ("Visit https://github.com/settings/tokens to create your GitHub token. It must have the 'repo' scope. Select the expiration date of your choice. Once expired, generate a new one and use this command to update it. We strongly recommend to create a 'classic' token named 'Alice Adventures'.")
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
      Show    : aliased Boolean := False;
      Refresh : aliased Boolean := False;
      Token   : aliased Boolean := False;
      License : aliased Boolean := False;
   end record;

end Alice_Cmd.Setup.Config;
