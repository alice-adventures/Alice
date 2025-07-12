-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.VCS.Service.GitHub;

package body Alice.App.Cmd.Profile.Token is

   function Run
     (Self : in out Object; Args : String := "")
      return Alice.VCS.Profile.Result.Object'Class
   is
      GitHub_Service : Alice.VCS.Service.GitHub.Object;
   begin
      return GitHub_Service.Get_Member_Profile_From_Token (Args);
   end Run;

end Alice.App.Cmd.Profile.Token;
