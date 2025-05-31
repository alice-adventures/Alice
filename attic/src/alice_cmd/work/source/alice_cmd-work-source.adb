-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO;

with AAA.Table_IO;
with AAA.Text_IO;
with Simple_Logging;

with Alice_Alire;

package body Alice_Cmd.Work.Source is

   package Log renames Simple_Logging;

   --------------------
   -- Setup_Switches --
   --------------------

   overriding procedure Setup_Switches
     (Cmd    : in out Cmd_Type;
      Config : in out CLIC.Subcommand.Switches_Configuration)
   is
   begin
      --!pp off
      CLIC.Subcommand.Define_Switch
        (Config      => Config,
         Output      => Cmd.List'Access,
         Switch      => "-l",
         Long_Switch => "--list",
         Help        => "Show available Problem Sources");

      CLIC.Subcommand.Define_Switch
        (Config      => Config,
         Output      => Cmd.Init'Access,
         Switch      => "-i",
         Long_Switch => "--init",
         Help        => "Initialize a Problem Source");

      CLIC.Subcommand.Define_Switch
        (Config      => Config,
         Output      => Cmd.Status'Access,
         Switch      => "-s",
         Long_Switch => "--status",
         Help        => "Show the overall status of a Problem Source");

      CLIC.Subcommand.Define_Switch
        (Config      => Config,
         Output      => Cmd.Update'Access,
         Switch      => "-u",
         Long_Switch => "--update",
         Help        => "Update Problem Source libraries and tools");
      --!pp on
   end Setup_Switches;

   ------------------
   -- Execute_List --
   ------------------

   procedure Execute_List is
      use Ada.Text_IO;
      Table : AAA.Table_IO.Table;
   begin
      Put_Line ("Available Problem Sources");
      Put_Line ("");
      Table.Append ("  Name").Append ("Id").Append ("Tag").Append ("URL")
        .New_Row;
      Table.Append ("  ----").Append ("--").Append ("---").Append ("---")
        .New_Row;
      for Source of Available_Sources loop
         Table.Append ("  " & To_String (Source.Name)).Append
           (To_String (Source.Id))
           .Append
           (To_String (Source.Tag))
           .Append
           (To_String (Source.URL))
           .New_Row;
      end loop;
      Table.Print ("    ");

      Put_Line ("");

      pragma Style_Checks (off);

      AAA.Text_IO.Put_Paragraph
        ("Note: when required, use the Id or the tag in alice commands to refer to a particular Problem Source");

      pragma Style_Checks (on);
   end Execute_List;

   --------------------------------
   -- Execute_Init_Project_Euler --
   --------------------------------

   procedure Execute_Init_Project_Euler is separate;

   ------------------
   -- Execute_Init --
   ------------------

   procedure Execute_Init (Source : String) is
   begin
      Alice_Alire.Ensure_Alice_Index;

      case Source is
         when "per" | "project_euler" =>
            Execute_Init_Project_Euler;
         when others =>
            Log.Error ("System error, unknown source '" & Source & "'");
      end case;
   end Execute_Init;

   --------------------
   -- Execute_Status --
   --------------------

   procedure Execute_Status (Source : String) is null;

   --------------------
   -- Execute_Update --
   --------------------

   procedure Execute_Update (Source : String) is null;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Cmd : in out Cmd_Type; Args : AAA.Strings.Vector)
   is
      Actions     : Natural          := 0;
      Args_Length : constant Natural := Natural (Args.Length);
   begin
      Actions := @ + (if Cmd.List then 1 else 0);
      Actions := @ + (if Cmd.Init then 1 else 0);
      Actions := @ + (if Cmd.Status then 1 else 0);
      Actions := @ + (if Cmd.Update then 1 else 0);

      if Actions = 0 then
         --  default command is '--list'
         Cmd.List := True;
      else
         Check_Unique_Subcommand (Actions);
      end if;

      if Cmd.Init or else Cmd.Status or else Cmd.Update then
         Check_Argument_Length (Args_Length, 1);
         if not Is_Valid_Source (Args.First_Element) then
            raise Command_Source_Error
              with "Invalid Source Id or tag, see 'source --list'";
         end if;
      end if;

      if Cmd.List then
         Execute_List;
      elsif Cmd.Init then
         Execute_Init (Args.First_Element);
      elsif Cmd.Status then
         Execute_Status (Args.First_Element);
      elsif Cmd.Update then
         Execute_Update (Args.First_Element);
      else
         Subcommand_Not_Found;
      end if;

   end Execute;

end Alice_Cmd.Work.Source;
