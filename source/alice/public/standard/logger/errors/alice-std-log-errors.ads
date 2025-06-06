-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package is an extension of the Alice logging system. It is not
--  intended for public use and should only be used by an error handler. Error
--  and Warning levels are reserved for an Error Handler that's in charge to
--  decide what's next, such as terminating the application or continuing with
--  a retry.
--
--  The logging levels are defined as follows:
--
--     * Error is used for errors that should not happen or exceptions caught
--
--     * Warning is used for conditions that are not errors but may cause
--       issues in the future, or for deprecated features

with Alice.IFace.Logger.Errors;

package Alice.Std.Log.Errors is

   type Object is new Alice.IFace.Logger.Errors.Object with null record;

   overriding
   procedure Error
     (Self     : in out Object;
      Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location);
   --  Log an error message: errors that should not happen or exceptions
   --  caught. The message is sent to the standard output when optimized for
   --  CLI, otherwise it is sent to the standard error.

   overriding
   procedure Warning
     (Self     : in out Object;
      Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location);
   --  Log a warning message: conditions that are not errors but may cause
   --  issues in the future, or for deprecated features. The message is sent
   --  to the standard error.

end Alice.Std.Log.Errors;
