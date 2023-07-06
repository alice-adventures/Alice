-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with OS_Cmd.Git; use OS_Cmd.Git;
with OS_Cmd.Alr; use OS_Cmd.Alr;

with GNAT.OS_Lib;
with Simple_Logging;
with Text_IO;

use all type GNAT.OS_Lib.String_Access;

package body Alice_Cmd.Setup.Check is

   package Log renames Simple_Logging;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Cmd : in out Cmd_Type; Args : AAA.Strings.Vector)
   is
      OS_Cmd_Alr : OS_Cmd_Alr_Type;
      OS_Cmd_Git : OS_Cmd_Git_Type;
      Args_Length : constant Natural := Natural (Args.Length);
   begin

      if Args_Length > 0 then
         Log.Warning ("Too many arguments, ignored");
      end if;

      OS_Cmd_Alr.Init;
      Text_IO.Put_Line ("found alr at '" & OS_Cmd_Alr.Path & "'");

      OS_Cmd_Git.Init;
      Text_IO.Put_Line ("found git at '" & OS_Cmd_Git.Path & "'");

      Text_IO.Put_Line ("");

   end Execute;

end Alice_Cmd.Setup.Check;
