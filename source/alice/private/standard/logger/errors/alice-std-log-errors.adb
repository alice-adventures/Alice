-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Simple_Logging;

package body Alice.Std.Log.Errors is

   -----------
   -- Error --
   -----------

   overriding
   procedure Error
     (Self     : in out Object;
      Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location) is
   begin
      Simple_Logging.Error (Msg, Entity, Location);
   end Error;

   -------------
   -- Warning --
   -------------

   overriding
   procedure Warning
     (Self     : in out Object;
      Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location) is
   begin
      Simple_Logging.Warning (Msg, Entity, Location);
   end Warning;

end Alice.Std.Log.Errors;
