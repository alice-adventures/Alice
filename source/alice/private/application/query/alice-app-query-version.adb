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
     (Self : in out Object; Args : String := "")
      return Alice.Result.Object'Class is
   begin
      Self.Context.Log.Trace_Begin;
      Self.Answer (Alice_Config.Crate_Version);
      return Result : Alice.Result.Success_Object do
         Self.Context.Log.Trace_Return
           (Result'Image & " with version " & Self.Answer'Image);
      end return;
   end Run;

end Alice.App.Query.Version;
