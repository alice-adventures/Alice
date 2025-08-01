-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package defines the command use case for updating end SPDX Id in the
--  member profile. This is used to set the license for the work done by the
--  member in the Alice project.

package Alice.Use_Case.Cmd.Profile.SPDX is

   type Object is new Alice.Use_Case.Cmd.Profile.Object with null record;

   overriding
   function Run
     (Self : in out Object; SPDX_Id : String) return Alice.Result.Object'Class;

end Alice.Use_Case.Cmd.Profile.SPDX;
