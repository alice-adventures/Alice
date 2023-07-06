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

   type OS_Cmd_Type is abstract tagged record
      OS_Path : GNAT.OS_Lib.String_Access;
   end record;

   procedure Init (Cmd : in out OS_Cmd_Type) is abstract;
   function Path (Cmd : OS_Cmd_Type) return String is (Cmd.OS_Path.all);

private

   procedure Init (Cmd : in out OS_Cmd_Type; Cmd_Name : String);

end OS_Cmd;
