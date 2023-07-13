-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.OS_Lib;
use all type GNAT.OS_Lib.String_Access;

generic
   OS_Cmd_Name : Unbounded_String;

package OS_Cmd is

   type Cmd_Type is limited private;

   type Run_Output_Type is record
      Return_Code : Integer;
      Temp_FD     : GNAT.OS_Lib.File_Descriptor;
      Temp_File   : GNAT.OS_Lib.String_Access;
   end record;

   function Init
     (Cmd : in out Cmd_Type; Report_Error : Boolean := True) return Boolean;
   --  Initialize an OS command by trying to find the executable file in
   --  PATH.

   function Path (Cmd : Cmd_Type) return String;
   --  Return the PATH where the OS command is found.

   function Run (Cmd : Cmd_Type; Args : String) return Run_Output_Type;
   --  Run the command with the given arguments.

   procedure Clean (Cmd : in out Cmd_Type; Run_Output : out Run_Output_Type);
   --  Run an OS command with the given arguments.

private

   type Cmd_Type is limited record
      OS_Path : aliased GNAT.OS_Lib.String_Access := null;
   end record;

end OS_Cmd;
