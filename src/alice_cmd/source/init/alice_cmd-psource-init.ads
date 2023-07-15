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
     ("<problem_source>");

   overriding function Short_Description (Cmd : Cmd_Type) return String is
     ("Initialize new Problem Source");

   --!pp off
   pragma Style_Checks (off);

   overriding function Long_Description
     (Cmd : Cmd_Type) return AAA.Strings.Vector is
     (AAA.Strings.Empty_Vector
         .Append ("Initializes a new Problem Source directory, populating it with all required repositories and resources.")
         .New_Line
         .Append ("• Installs the repository 'alice-adventures/<problem_source>', which is, in turn, the crate '<problem_source>' in the 'alice' index (cloned with 'git', maintained with 'alr').")
         .New_Line
         .Append ("• Creates a new git repository in your GitHub account. This repository is named 'alice-<problem_source>', which is cloned from the repository 'alice-adventures/<problem_source>-template' repository.")
         .New_Line
         .Append ("• Your new repository is created in '<problem_source>/usr/<your_login>/'.")
         .New_Line
         .Append ("• Clone the repository '<problem_source>-share' to access all the required resources. This repository is cloned in '<problem_source>/share'.")
         .New_Line
         .Append ("Note: Check all the available Problem Sources with 'alice list'.")
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
