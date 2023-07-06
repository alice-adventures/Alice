-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with AAA.Strings;
with CLIC.Subcommand;

package Alice_Cmd.PSource.Init is

   type Cmd_Type is new CLIC.Subcommand.Command with null record;

   overriding function Name
     (Cmd : Cmd_Type) return CLIC.Subcommand.Identifier is
     ("init");

   overriding function Usage_Custom_Parameters
     (Cmd : Cmd_Type) return String is
     ("[Problem Source Tag]");

   overriding function Short_Description (Cmd : Cmd_Type) return String is
     ("Initialize new Problem Source");

   --!pp off
   pragma Style_Checks (off);

   overriding function Long_Description
     (Cmd : Cmd_Type) return AAA.Strings.Vector is
     (AAA.Strings.Empty_Vector
         .Append ("Creates a new Problem Source directory, populating it with all required files and libraries.")
         .Append ("Retrieves the necessary resources from the Problem Source shared repository.")
         .Append ("With new Problem Source directory a new GitHub repository is created for you to store and share all solved problems.")
         .Append ("Check all the available Problem Sources with 'alice list'.")
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

private

   function Project_Euler return Boolean;

end Alice_Cmd.PSource.Init;
