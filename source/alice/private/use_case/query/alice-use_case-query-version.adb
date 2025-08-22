-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice_Config;

package body Alice.Use_Case.Query.Version is

   ---------
   -- Run --
   ---------

   overriding
   function Run
     (Self : in out Object; Args : String := "")
      return Alice.Result.Object_With_Data'Class is
   begin
      Self.Context.Log.Trace_Begin;
      return
         Result : constant Alice.Result.Object_With_Data'Class :=
           Alice.Result.Success_With_Data
             (Alice.UStr ("Version: " & Alice_Config.Crate_Version))
      do
         Self.Context.Log.Trace_Return (Result'Image);
      end return;
   end Run;

end Alice.Use_Case.Query.Version;
