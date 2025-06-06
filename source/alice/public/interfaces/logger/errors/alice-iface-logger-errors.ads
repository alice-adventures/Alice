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

package Alice.IFace.Logger.Errors is

   type Object is interface;
   --  The interface for the Error Logger. It provides methods to log
   --  error and warning messages. This interface is intended to be used by
   --  an error handler to log errors and warnings that occur during the
   --  execution of use cases or other operations in the Alice application.

   type Object_Access is not null access all Object;
   --  The access type for the Error Logger Object.

   procedure Error
     (Self     : in out Object;
      Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location)
   is abstract;
   --  Log an error message: errors that should not happen or exceptions
   --  caught.

   procedure Warning
     (Self     : in out Object;
      Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location)
   is abstract;
   --  Log a warning message: conditions that are not errors but may cause
   --  issues in the future, or for deprecated features.

end Alice.IFace.Logger.Errors;
