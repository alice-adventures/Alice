-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package defines the interface for all use cases in the Alice
--  application. It provides a common structure for running use cases and
--  handling their results. Each use case must implement the `Run` method,
--  which takes an application context and returns a result of type
--  `Alice.Result.Object'Class`. This allows for a consistent way to execute
--  use cases and handle their outcomes, whether they succeed or fail.
--  According to the CQRS (Command Query Responsibility Segregation) pattern,
--  use cases can be categorized into commands (which change state) and
--  queries (which retrieve data). This package serves as the foundation for
--  implementing both types of use cases in the Alice application.

with Ada.Exceptions;
pragma Unreferenced (Ada.Exceptions);

with Alice.Context;
with Alice.Result;

package Alice.App is

   type Use_Case is interface;
   --  Use_Case is an interface that defines the structure for all use cases
   --  in the Alice application. It is expected that use cases will extend
   --  this interface to add additional parameters as needed.

   function Run
     (Self : Use_Case; Ctx : Alice.Context.Object)
      return Alice.Result.Object'Class
   is abstract;
   --  This function is an abstract method that must be implemented by any
   --  concrete use case type. It is expected to execute the use case logic
   --  and return a result of type Alice.Result.Object'Class.
   --
   --  #FIXME - What is an actual Use Case needs additional parameters? Makes
   --  sense to derive from the Use_Case interface and add the parameters
   --  to the Run function? Or should we use a different approach, like a
   --  configuration object or a context object that contains all necessary
   --  parameters for the use case?

end Alice.App;
