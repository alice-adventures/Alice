-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package defines the interface for error handlers in the ALICE
--  application. It allows different implementations to handle errors in a
--  consistent manner. The error handler is responsible for processing results
--  of type Alice.Result.Object'Class, which encapsulates the outcome of an
--  operation, including any errors that may have occurred.

with Alice.Result;

package Alice.IFace.Error_Handler is

   type Object is interface;
   --  This interface defines the contract for error handlers in the ALICE
   --  application. It allows different implementations to handle errors in a
   --  consistent manner.

   type Object_Access is not null access all Object'Class;
   --  Object_Access is an access type for the Object interface. It allows
   --  for dynamic dispatch and polymorphism, enabling different error handler
   --  implementations to be used interchangeably.

   function Handle_Error
     (Self : Object; Result : Alice.Result.Object_Access)
      return Boolean
   is abstract;
   --  This function is an abstract method that must be implemented by any
   --  concrete error handler type. It is expected to handle the provided
   --  error and, if the error is recoverable, return True to indicate that
   --  the operation can be retried. If the error is not recoverable, it should
   --  return False to indicate that the operation cannot be retried and the
   --  application should terminate or take appropriate action.

end Alice.IFace.Error_Handler;
