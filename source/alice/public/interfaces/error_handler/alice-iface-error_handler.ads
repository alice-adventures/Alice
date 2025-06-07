-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package defines the interface for error handlers in the Alice
--  application. It allows different implementations to handle errors in a
--  consistent manner. The error handler is responsible for processing results
--  of type Alice.Result.Object'Class, which encapsulates the outcome of an
--  operation, including any errors that may have occurred.
--
--  It is expected that the error handler will determine whether an error is
--  recoverable or not. If the error is recoverable, the Handle_Error function
--  should return True, indicating that the operation can be retried. If the
--  error is not recoverable, the handler should return False, indicating that
--  the operation cannot be retried and the application should take
--  appropriate action, such as terminating or logging the error.

with Alice.Result;

package Alice.IFace.Error_Handler is

   type Object is interface;
   --  This interface defines the contract for error handlers in the Alice
   --  application. It allows different implementations to handle errors in a
   --  consistent manner.

   type Object_Access is not null access all Object'Class;
   --  Object_Access is an access type for the Object interface. It allows
   --  for dynamic dispatch and polymorphism, enabling different error handler
   --  implementations to be used interchangeably.

   type Exit_Code_Value is (Trap, Bug, Success, Error, System, External);
   for Exit_Code_Value use
     (Trap     => -2,
      Bug      => -1,
      Success  => 0,
      Error    => 1,
      System   => 3,
      External => 4);
   --  Exit_Code represents the exit code associated with an error or
   --  exception. When Success, it is not set because is always 0.
   --
   --  Values are defined as follows:
   --    * Trap:    -2, indicates a trap or assertion failure.
   --    * Bug:     -1, indicates a bug in the code.
   --    * Error:    1, indicates a general error, usually at Domain level.
   --    * System:   2, indicates a system-level error.
   --    * External: 3, indicates an external error, such as a network failure
   --      or an external API error.
   --
   --  It is expected that the Error_Handler implementation will use these
   --  exit codes when exiting the application or when reporting an error to
   --  the user.

   function Handle_Error
     (Self : Object; Result : Alice.Result.Object_Access) return Boolean
   is abstract;
   --  Handle the provided error and, if the error is recoverable, return True
   --  to indicate that the operation can be retried. If the error is not
   --  recoverable, it should return False to indicate that the operation
   --  cannot be retried and the application should terminate or take
   --  appropriate action.
   --
   --  Sometimes, the error handler may also terminate the application if the
   --  error is a bug or is not recoverable, in which case it should call the
   --  Exit_Application procedure with the appropriate result.
   --
   --  Usually, Trap, Bug, and System errors are not recoverable, while
   --  Domain and External errors may be recoverable depending on the context
   --  and the specific error handler implementation.
   --
   --  This method should log the error using the appropriate logging
   --  mechanism, or logging the error to a file or external service,
   --  depending on the application's logging configuration and the error
   --  handler's implementation.

   procedure Exit_Application
     (Self : Object; Result : Alice.Result.Object_Access)
   is abstract;
   --  Exit the application with the provided result. This procedure is called
   --  when the application determines that should be terminated due to a
   --  non-recoverable error. It allows the application to perform any
   --  necessary cleanup or logging before exiting. Implementations of this
   --  procedure should use the appropriate exit code based on the result
   --  status, such as 0 for success or a non-zero value for errors, as
   --  defined by Exit_Code_Value.

end Alice.IFace.Error_Handler;
