-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package is the top-level package for the Alice interfaces. According
--  to the Ada 2022 standard, it is recommended to use a single package for
--  all interfaces in a project. This package serves as the main entry point
--  for all interfaces in the Alice application.

with Alice.Result;

package Alice.IFace is

   type Object is interface;

   function Initialize (Self : in out Object) return Alice.Result.Object'Class
   is abstract;
   --  This procedure should be called before using any other procedures or
   --  functions in the interface. It sets up the necessary context and
   --  resources for the interface implementation to function correctly.

   procedure Finalize (Self : in out Object) is abstract;
   --  This procedure should be called when the application is shutting down
   --  to clean up resources.

end Alice.IFace;
