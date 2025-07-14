-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.Result;
with Alice.VCS.Service.GitHub;

package body Test.VCS_Service is

   procedure Run (Ctx : Alice.Context.Object_Access) is
      GitHub : Alice.VCS.Service.GitHub.Object;
   begin
      Result : Alice.Result.Object'Class :=
        GitHub.Get_Member_Profile_From_Token
          ("INVALID-TOKEN");
          --  #TODO - read from config file of from command line
   end Run;

end Test.VCS_Service;
