-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package body Alice.Use_Case is

   -------------
   -- Context --
   -------------

   overriding
   function Context (Self : in out Object) return Alice.Context.Object_Access
   is (Self.Context);

   -------------
   -- Context --
   -------------

   overriding
   procedure Set_Context
     (Self : in out Object; Context : Alice.Context.Object_Access) is
   begin
      Self.Context := Context;
   end Set_Context;

end Alice.Use_Case;
