-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package is the top-level package for the query use cases in the Alice
--  application.

with Alice.App.Use_Case;
with Alice.Result;

package Alice.App.Query.Use_Case is

   type Object is new Alice.App.Use_Case.Object with private;

   function Answer (Self : Object) return Alice.UString;

   procedure Answer
     (Self : in out Object; Value : Alice.UString);

   overriding
   function Run
     (Self : in out Object; Args : String := "")
      return Alice.Result.Object'Class;

private

   type Object is new Alice.App.Use_Case.Object with record
      Answer : Alice.UString := Alice.UStr ("");
   end record;

end Alice.App.Query.Use_Case;
