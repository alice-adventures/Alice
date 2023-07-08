-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GNAT.OS_Lib;
use all type GNAT.OS_Lib.String_Access;

package OS_Cmd is

   type OS_Cmd_Type is abstract tagged limited private;

   type Run_Output_Type is record
      Return_Code : Integer;
      Temp_FD     : GNAT.OS_Lib.File_Descriptor;
      Temp_File   : GNAT.OS_Lib.String_Access;
   end record;

   procedure Init (Cmd : in out OS_Cmd_Type) is abstract;

   function Path (Cmd : OS_Cmd_Type) return String;

   function Run (Cmd : OS_Cmd_Type; Args : String) return Run_Output_Type;
   procedure Clean
     (Cmd : in out OS_Cmd_Type; Run_Output : out Run_Output_Type);

private

   type OS_Cmd_Type is abstract tagged limited record
      OS_Path : GNAT.OS_Lib.String_Access;
   end record;

   procedure Init (Cmd : in out OS_Cmd_Type; Cmd_Name : String);

end OS_Cmd;
