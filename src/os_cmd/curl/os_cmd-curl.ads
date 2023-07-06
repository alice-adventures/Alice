-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package OS_Cmd.Curl is

   type OS_Cmd_Curl_Type is new OS_Cmd_Type with null record;

   overriding procedure Init (OS_Cmd : in out OS_Cmd_Curl_Type);

end OS_Cmd.Curl;
