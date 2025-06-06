-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Exceptions;
pragma Unreferenced (Ada.Exceptions);

with Alice.Context;
with Alice.Result;

package Alice.App is

   type Use_Case is interface;

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
