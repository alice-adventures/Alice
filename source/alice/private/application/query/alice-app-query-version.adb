-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice_Config;

package body Alice.App.Query.Version is

   ---------
   -- Run --
   ---------

   overriding
   function Run
     (Self : in out Use_Case; Ctx : Alice.Context.Object)
      return Alice.Result.Object'Class
   is
      Version : constant Result :=
        (Status  => Alice.Result.Success,
         Version =>
           (if Self.Full_Text
            then UStr ("ALICE CRATE VERSION is " & Alice_Config.Crate_Version)
            else UStr (Alice_Config.Crate_Version)));
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
               Message => Alice.UStr (Ada.Exceptions.Exception_Message (E)));
   end Run;

end Alice.App.Query.Version;
