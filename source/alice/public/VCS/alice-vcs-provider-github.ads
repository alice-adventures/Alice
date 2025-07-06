-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.VCS.Profile;
with Alice.VCS.Profile.Result;
with Alice.IFace.VCS.Provider;

package Alice.VCS.Provider.GitHub is

   type Object is new Alice.Controlled and Alice.IFace.VCS.Provider.Object
   with null record;

   type Object_Access is access all Object'Class;

   overriding
   function Get_Profile_From_Token
     (Self : in out Object; Token : String)
      return Alice.VCS.Profile.Result.Object'Class;

   overriding
   function Get_Profile_From_VCS_Config_File
     (Self : in out Object; Token : String)
      return Alice.VCS.Profile.Result.Object'Class;

   overriding
   function Get_Profile_From_Alice_Config_File
     (Self : in out Object; File : String)
      return Alice.VCS.Profile.Result.Object'Class;

end Alice.VCS.Provider.GitHub;
