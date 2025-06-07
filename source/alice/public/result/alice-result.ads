-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package defines the Result type used to represent the outcome of
--  operations in the Alice application. It includes a status type, error
--  level, and a tagged record to encapsulate the result of an operation,
--  including success and error cases.

package Alice.Result is

   type Status_Type is (Success, Error);
   --  Status_Type represents the outcome of an operation. It can be:
   --    * Success: The operation completed successfully.
   --    * Error: The operation encountered an error.
   --    * Caught_Exception: The operation was interrupted by an exception
   --      that was caught and handled.
   --      This is used to distinguish between expected errors and unexpected
   --      exceptions that were caught during the operation.

   type Error_Level is (Bug, Domain, System, External);
   --  Level Bug refers to software errors like:
   --     * Null pointers
   --     * Invalid internal state
   --
   --  Level Domain refers to errors like:
   --     * Business rule violations
   --     * Invalid user input
   --     * Resource not found
   --     * Validation failures
   --
   --  Level System refers to errors like:
   --     * Exceptions raised by the Ada runtime
   --     * Permission denied (e.g. at filesystem level)
   --     * File not found
   --     * Unable to read/write file
   --
   --  Level External refers to errors like:
   --     * No network connection
   --     * External API error
   --     * Invalid GitHub account (e.g., authentication failure, account
   --       suspension)

   type Object (Status : Status_Type) is tagged record
      case Status is
         when Success =>
            null;

         when Error =>
            Level   : Error_Level;
            Message : UString;
      end case;
   end record;
   --  If the status is Success, no additional fields are present. Alice
   --  operations can extend the record with additional result fields when
   --  Success. For example:
   --
   --     type Extended_Result_Object is new Object with record
   --        case Status is
   --           when Success =>
   --              Data : Integer;
   --
   --           when others =>
   --              null;
   --        end case;
   --     end record;
   --
   --  Developers should ensure that extensions are meaningful and consistent
   --  with the operation's outcome.

   subtype Success_Object is Object (Success);
   --  Success_Object is a subtype of Object that represents a successful
   --  operation. It does not contain any additional fields, as it is used to
   --  indicate that the operation completed successfully without any errors.

   subtype Error_Object is Object (Error);
   --  Error_Object is a subtype of Object that represents an operation that
   --  encountered an error. It contains the Level and Message fields to
   --  provide additional information about the error that occurred.

end Alice.Result;
