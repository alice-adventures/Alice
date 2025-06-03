-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package defines the Result type used to represent the outcome of
--  operations in the ALICE application. It includes a status type, error
--  context, and a tagged record to encapsulate the result of an operation,
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
   --  Context Bug refers to software errors like:
   --     * Null pointers
   --     * Invalid internal state
   --
   --  Context Domain refers to errors like:
   --     * Business rule violations
   --     * Invalid user input
   --     * Resource not found
   --     * Validation failures
   --
   --  Context System refers to errors like:
   --     * Exceptions raised by the Ada runtime
   --     * Permission denied (e.g. at filesystem level)
   --     * File not found
   --     * Unable to read/write file
   --
   --  Context External refers to errors like:
   --     * No network connection
   --     * External API error
   --     * Invalid GitHub account (e.g., authentication failure, account
   --       suspension)

   type Exit_Code_Value is (Bug, Error, Trap, System, External);
   for Exit_Code_Value use
     (Bug => -1, Error => 1, Trap => 2, System => 3, External => 4);
   --  Exit_Code represents the exit code associated with an error or
   --  exception. When Success, it is not set because is always 0.
   --
   --  Values are defined as follows:
   --    * Bug: -1, indicates a bug in the code.
   --    * Error: 1, indicates a general error, usually at Domain level.
   --    * Trap: 2, indicates a trap or assertion failure.
   --    * System: 3, indicates a system-level error.
   --    * External: 4, indicates an external error, such as a network failure
   --      or an external API error.
   --
   --  It is expected that the Error_Handler will use these exit codes when
   --  stopping the application or when reporting an error to the user. The
   --  exit codes are not mandatory to set for every error or exception, but
   --  they can provide additional context for an error handler to understand
   --  the nature of the error or exception.

   type Object (Status : Status_Type) is tagged record
      case Status is
         when Success =>
            null;

         when Error =>
            Level     : Error_Level;
            Message   : UString;
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
   --           when others =>
   --              null;
   --        end case;
   --     end record;
   --
   --  It is expected that the caller will check the status before accessing
   --  the additional fields. Developers should ensure that extensions are
   --  meaningful and consistent with the operation's outcome.
   --
   --  Exit_Code represents the exit code associated with the error or
   --  exception. It can be a system exit code, an application-specific code,
   --  or a custom code defined by the operation that encountered the error.
   --  It is not mandatory to set Exit_Code for every error or exception, but
   --  it can provide additional context for an error handler to understand
   --  the nature of the error or exception.

end Alice.Result;
