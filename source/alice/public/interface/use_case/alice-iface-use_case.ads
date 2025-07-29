-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package defines the interface for all use cases in the Alice
--  application. It provides a common structure for managing use cases.
--
--  According to the CQRS (Command/Query Responsibility Segregation) pattern,
--  use cases can be categorized into commands (which change state) and
--  queries (which retrieve data). This package serves as the foundation for
--  implementing both types of use cases in the Alice application.

with Alice.Context;

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

   procedure Set_Context
     (Self : in out Object; Context : Alice.Context.Object_Access)
   is abstract;
   --  This procedure sets the context for the use case. It allows the use
   --  case to be associated with a specific application context, which
   --  provides the necessary data and services for the use case to operate.
   --  This is typically called during the initialization phase of the use
   --  case, ensuring that it has access to the required context when
   --  executing its logic.

end Alice.IFace.Use_Case;
