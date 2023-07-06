-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package body Alice_Cmd.PSource is

   function Is_Valid_Tag (Tag : Unbounded_String) return Boolean is
   begin
      for PSource of Available_PSources loop
         if PSource.Tag = Tag then
            return True;
         end if;
      end loop;

      return False;
   end Is_Valid_Tag;

end Alice_Cmd.PSource;
