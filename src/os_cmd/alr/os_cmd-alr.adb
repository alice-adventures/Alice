-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package body OS_Cmd.Alr is

   ----------
   -- Init --
   ----------

   overriding function Init
     (Cmd : in out Alr_Cmd_Type; Report_Error : Boolean := True) return Boolean
   is
   begin
      return Cmd.Init ("alr", Report_Error);
   end Init;

end OS_Cmd.Alr;
