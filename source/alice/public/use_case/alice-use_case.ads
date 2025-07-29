-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package defines the top-level package for the implementation of Alice
--  Use Cases. It provides an abstract type for Use Cases, which can be
--  extended to create specific Use Case implementations. The package also
--  defines the context in which the Use Cases operate, allowing them to
--  access shared resources and configurations.

with Alice.Context;
with Alice.IFace.Use_Case;
with Alice.Std;

package Alice.Use_Case is

   type Object is abstract new Alice.IFace.Use_Case.Object with private;

   overriding
   function Context
     (Self : in out Object) return Alice.Context.Object_Access;

   overriding
   procedure Set_Context
     (Self : in out Object; Context : Alice.Context.Object_Access);

private

   type Object is new Alice.IFace.Use_Case.Object with record
      Context : Alice.Context.Object_Access := Alice.Std.Get_Context;
   end record;

end Alice.Use_Case;
