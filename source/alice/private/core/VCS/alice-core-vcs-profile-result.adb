-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package provides the Result type for operations related to VCS
--  profiles in the Alice application. It defines a tagged record that
--  encapsulates the result of operations, including success and error cases.

with Ada.Unchecked_Deallocation;

package body Alice.Core.VCS.Profile.Result is

   ----------
   -- Free --
   ----------

   procedure Free is new
     Ada.Unchecked_Deallocation
       (Alice.Core.VCS.Profile.Object,
        Alice.Core.VCS.Profile.Object_Access);

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Self : in out Object) is
   begin
      case Self.Status is
         when Alice.Result.Success =>
            Free (Self.Profile);

         when Alice.Result.Error =>
            null;
      end case;
   end Finalize;

end Alice.Core.VCS.Profile.Result;
