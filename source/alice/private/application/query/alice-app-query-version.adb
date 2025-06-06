-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package contains the implementation of the version query use case. It
--  resolves the current version of the application and returns it as a
--  result.

with Alice_Config;

package body Alice.App.Query.Version is

   overriding
   function Run
     (Self : Use_Case; Ctx : Alice.Context.Object)
      return Alice.Result.Object'Class
   is
      Version : constant Result :=
        (Status  => Alice.Result.Success,
         Version => U (Alice_Config.Crate_Version));
   begin
      Ctx.Log.Trace_Begin;
      Ctx.Log.Trace_Return (Version'Image);
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
