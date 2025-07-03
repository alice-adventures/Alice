-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package serves as the main entry point for all interfaces in the
--  Alice application.

package Alice.IFace is

   type Object is interface;

   procedure Initialize (Self : in out Object) is null;
   --  This procedure should be called before using any other procedures or
   --  functions in the interface. It sets up the necessary context and
   --  resources for the interface implementation to function correctly.
   --  Default implementation does nothing, but can be overridden by specific
   --  implementations.
   --
   --  In case of error in an overridden implementation, an exception should
   --  be raised to indicate the failure to initialize the interface.

   procedure Finalize (Self : in out Object) is null;
   --  This procedure should be called when the application is shutting down
   --  to clean up interface resources.

end Alice.IFace;
