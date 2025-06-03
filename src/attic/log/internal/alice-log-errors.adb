-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Simple_Logging;

package body Alice.Log.Errors is

   -----------
   -- Error --
   -----------

   procedure Error
     (Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location) renames Simple_Logging.Error;

   -------------
   -- Warning --
   -------------

   procedure Warning
     (Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location) renames Simple_Logging.Warning;

end Alice.Log.Errors;
