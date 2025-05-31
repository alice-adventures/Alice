-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package body Alice_Cmd.Work is

   ---------------------
   -- Is_Valid_Source --
   ---------------------

   function Is_Valid_Source (Id_Or_Tag : Unbounded_String) return Boolean is
   begin
      for Source of Available_Sources loop
         if Source.Id = Id_Or_Tag or else Source.Tag = Id_Or_Tag then
            return True;
         end if;
      end loop;

      return False;
   end Is_Valid_Source;

end Alice_Cmd.Work;
