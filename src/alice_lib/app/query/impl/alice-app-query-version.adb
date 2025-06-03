-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice_Config;

with Alice.Log;

package body Alice.App.Query.Version is

   overriding
   function Run (Self : Use_Case) return Alice.Result.Object'Class is
      Version : constant Result :=
        (Status  => Alice.Result.Success,
         Version => U (Alice_Config.Crate_Version));
   begin
      Alice.Log.Trace_Begin;
      Alice.Log.Trace_Return (Version'Image);
      return Version;

   exception
      when E : others =>
         return
            R : constant Result :=
              (Status  => Alice.Result.Error,
               Level   => Alice.Result.System,
               Message => U (Ada.Exceptions.Exception_Message (E)));
   end Run;

end Alice.App.Query.Version;
