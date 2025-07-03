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
--
--  According to the CQRS (Command/Query Responsibility Segregation) pattern,
--  use cases can be categorized into commands (which change state) and
--  queries (which retrieve data). This package serves as the foundation for
--  implementing both types of use cases in the Alice application.

with Alice.Context;
with Alice.Result;

package Alice.IFace.Use_Case is

   type Object is interface and Alice.IFace.Object;
   --  Defines the structure for all use cases in the Alice application. It is
   --  expected that use cases will extend this interface to add additional
   --  parameters as needed.

   function Context (Self : in out Object) return Alice.Context.Object_Access
   is abstract;
   --  This function returns the context associated with the use case. The
   --  context provides access to application-specific data and services that
   --  the use case may need to operate. It is expected that the context will
   --  be set during the initialization of the use case and can be used to
   --  retrieve information or perform actions relevant to the use case.

   procedure Context (Self : in out Object; Ctx : Alice.Context.Object_Access)
   is abstract;
   --  This procedure sets the context for the use case. It allows the use
   --  case to be associated with a specific application context, which
   --  provides the necessary data and services for the use case to operate.
   --  This is typically called during the initialization phase of the use
   --  case, ensuring that it has access to the required context when
   --  executing its logic.

   function Run
     (Self : in out Object; Args : String := "")
      return Alice.Result.Object'Class
   is abstract;
   --  This function must be implemented by any concrete use case type. It is
   --  expected to execute the use case logic and return a result of type
   --  Alice.Result.Object'Class. Additional, simple arguments can be passed
   --  to the use case, allowing for flexibility in how the use case is
   --  executed. In case of more complex parameters, it is recommended to use
   --  a record type, by extending the Use_Case record, to encapsulate the
   --  parameters rather than passing them as a string. This allows for better
   --  type safety and clarity in the use case's interface.

end Alice.IFace.Use_Case;
