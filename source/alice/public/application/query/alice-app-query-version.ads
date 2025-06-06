-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Alice.App.Query.Version is

   type Use_Case is new Alice.App.Use_Case with null record;
   --  This type represents the use case for querying the version of the Alice
   --  application. It inherits from the Alice.App.Use_Case interface.

   type Result (Status : Alice.Result.Status_Type) is
     new Alice.Result.Object (Status)
   with record
      case Status is
         when Alice.Result.Success =>
            Version : UString;

         when others =>
            null;
      end case;
   end record;
   --  This type represents the result of the version query.

   overriding
   function Run
     (Self : Use_Case; Ctx : Alice.Context.Object)
      return Alice.Result.Object'Class;
   --  This function retrieves the version of the Alice application.
   --  It returns a result with the version information.

end Alice.App.Query.Version;
