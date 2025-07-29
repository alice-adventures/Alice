-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.Context;
with Alice.Result;

package Alice.IFace.Use_Case.Cmd is

   type Object is interface and Alice.IFace.Use_Case.Object;

   overriding
   function Context
     (Self : in out Object) return Alice.Context.Object_Access is abstract;

   overriding
   procedure Set_Context
     (Self : in out Object; Context : Alice.Context.Object_Access) is abstract;

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

end Alice.IFace.Use_Case.Cmd;
