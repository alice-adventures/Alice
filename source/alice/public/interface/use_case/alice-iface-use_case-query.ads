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

--  with Ada.Strings.Text_Buffers;

with Alice.Result;

package Alice.IFace.Use_Case.Query is

   type Object is interface and Alice.IFace.Use_Case.Object;
   --  Defines the structure for all use cases in the Alice application. It is
   --  expected that use cases will extend this interface to add additional
   --  parameters as needed.

   overriding
   function Context
     (Self : in out Object) return Alice.Context.Object_Access is abstract;

   overriding
   procedure Set_Context
     (Self : in out Object; Context : Alice.Context.Object_Access) is abstract;

   function Run
     (Self : in out Object; Args : String := "")
      return Alice.Result.Object_With_Data'Class
   is abstract;
   --  This function must be implemented by any concrete use case type. It is
   --  expected to execute the use case logic and return a result of type
   --  Alice.Result.Object'Class. Additional, simple arguments can be passed
   --  to the use case, allowing for flexibility in how the use case is
   --  executed. In case of more complex parameters, it is recommended to use
   --  a record type, by extending the Use_Case record, to encapsulate the
   --  parameters rather than passing them as a string. This allows for better
   --  type safety and clarity in the use case's interface.

end Alice.IFace.Use_Case.Query;
