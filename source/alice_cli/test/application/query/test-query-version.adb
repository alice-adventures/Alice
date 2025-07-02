-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GNAT.Source_Info;

with Alice.App.Query.Version;
with Alice.Result;

package body Test.Query.Version is

   --------------------
   -- Return_Success --
   --------------------

   procedure Return_Success is
      Use_Case : Alice.App.Query.Version.Object;
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);

      Result : constant Alice.Result.Object'Class := Use_Case.Run;
      case Result.Status is
         when Alice.Result.Success =>
            Use_Case.Context.Log.Info (Alice.Str (Use_Case.Answer));
            Test.Pass;

         when Alice.Result.Error =>
            Test.Fail (Alice.Str (Result.Message));
      end case;
   end Return_Success;

   ---------
   -- Run --
   ---------

   procedure Run is
   begin
      Return_Success;
   end Run;

end Test.Query.Version;
