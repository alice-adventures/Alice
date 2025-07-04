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
   --
   --  Some interface implementations may derive from Ada.Controlled, in which
   --  case this procedure is called automatically by the Ada runtime when the
   --  object is created. Because of this, it is not necessary to call this
   --  procedure explicitly in the constructor of the interface
   --  implementation. But, if an access type is being used, it is necessary
   --  to call this procedure explicitly after the object is allocated, to
   --  ensure that the interface is properly initialized.

   procedure Finalize (Self : in out Object) is null;
   --  This procedure should be called when the application is shutting down
   --  to clean up interface resources.
   --
   --  If an implementation derives from Ada.Controlled, then this procedure
   --  is called automatically by the Ada runtime when the object is being
   --  deallocated, so it is not necessary to call it explicitly.

end Alice.IFace;
