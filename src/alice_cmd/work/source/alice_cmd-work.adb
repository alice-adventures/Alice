-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package body Alice_Cmd.Work is

   -------------------------
   -- Is_Valid_Source_Tag --
   -------------------------

   function Is_Valid_Source_Tag (Tag : Unbounded_String) return Boolean is
   begin
      for Source of Available_Sources loop
         if Source.Tag = Tag then
            return True;
         end if;
      end loop;

      return False;
   end Is_Valid_Source_Tag;

end Alice_Cmd.Work;
