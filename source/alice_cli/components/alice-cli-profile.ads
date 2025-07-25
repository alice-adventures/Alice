-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package contains the configuration subcommands for the Alice CLI
--  application.

with AAA.Strings;
with CLIC.Subcommand;

with Alice.Context;
with Alice.Std;

package Alice.CLI.Profile is

   type Object is new CLIC.Subcommand.Command with private;
   --  Command type for the configuration subcommands.

   overriding
   function Name (Self : Object) return CLIC.Subcommand.Identifier
   is ("profile");
   --  Name of the configuration command.

   overriding
   function Usage_Custom_Parameters (Self : Object) return String
   is ("[ --token <GH Token> |"
       & " --spdx {<Id>|list} |"
       & " --refresh ]");

   overriding
   function Short_Description (Self : Object) return String
   is ("Manage member profile");

   --!pp off
   pragma Style_Checks (off);

   overriding
   function Long_Description (Self : Object) return AAA.Strings.Vector
   is (AAA.Strings.Empty_Vector
      .Append ("Manage Alice member profile, which includes a GitHub token, login, name, email (if present) and the SPDX identifier for member's work.")
      .New_Line
      .Append ("The GitHub token enables interaction through the GitHub REST API. Your GitHub token exclusively created to work with Alice must be kept in secret. Do not shared it with other members and do not use it for other applications. It is stored in the user configuration file, which is ignored by git to not to push it accidentally (GitHub automatically revokes all pushed tokens).")
      .New_Line
      .Append ("Visit https://github.com/settings/tokens to create your GitHub token. It must have the 'repo' scope. Select the expiration date of your choice. Once expired, generate a new one and use this command to update it. We strongly recommend to create a 'classic' token named 'Alice Adventures'.")
      .New_Line
      .Append ("The SPDX license identifier is used to replace such Id in some source code templates. Work attribution is made with the member's name, SPDX license identifier and the current year. Each member is free to choose which license must apply to his/her work. By default 'MIT' license is applied. Choose any valid identifier from https://spdx.org/licenses that is compatible with software works (e.g. Creative Commons licenses are not recommended for software).")
      .New_Line
      .Append ("If you change some settings in your GitHub account (e.g. name, email), then use '--refresh' to update these changes in your config file, provided that the current token is still valid.")
   );

   pragma Style_Checks (on);
   --!pp on

   overriding
   function Switch_Parsing
     (Self : Object) return CLIC.Subcommand.Switch_Parsing_Kind
   is (CLIC.Subcommand.Parse_All);

   overriding
   procedure Setup_Switches
     (Self   : in out Object;
      Config : in out CLIC.Subcommand.Switches_Configuration);

   overriding
   procedure Execute (Self : in out Object; Args : AAA.Strings.Vector);

private

   type Config_Flags is record
      Show    : aliased Boolean := False; --  Show member configuration
      Token   : aliased Boolean := False; --  Set profile from GitHub token
      SPDX    : aliased Boolean := False; --  Set SPDX license ID
      Refresh : aliased Boolean := False; --  Refresh member configuration
   end record;

   type Object is new CLIC.Subcommand.Command with record
      Context : Alice.Context.Object_Access := Alice.Std.Get_Context;
      Flag    : Config_Flags;
   end record;

end Alice.CLI.Profile;
