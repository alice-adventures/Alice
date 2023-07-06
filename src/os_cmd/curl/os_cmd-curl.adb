-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package body OS_Cmd.Curl is

   ----------
   -- Init --
   ----------

   overriding procedure Init (OS_Cmd : in out OS_Cmd_Curl_Type) is
   begin
      OS_Cmd.Init ("curl");
   end Init;

end OS_Cmd.Curl;
