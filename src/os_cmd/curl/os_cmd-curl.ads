-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package OS_Cmd.Curl is

   type Curl_Cmd_Type is new OS_Cmd_Type with null record;

   overriding function Init
     (Cmd : in out Curl_Cmd_Type; Report_Error : Boolean := True)
      return Boolean;

end OS_Cmd.Curl;
