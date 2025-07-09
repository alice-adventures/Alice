-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.IFace.VCS.Service;
with Alice.Result;
with Alice.VCS.Profile;
with Alice.VCS.Profile.Result;

package Alice.VCS.Service.GitHub is

   type Object is new Alice.Controlled and Alice.IFace.VCS.Service.Object
   with null record;

   type Object_Access is access all Object'Class;

   overriding
   function Get_Member_Profile_From_Token
     (Self : in out Object; Token : String)
      return Alice.VCS.Profile.Result.Object'Class;

   overriding
   function Get_Member_Profile_From_VCS_Config_File
     (Self : in out Object; Token : String)
      return Alice.VCS.Profile.Result.Object'Class;

   overriding
   function Get_Member_Profile_From_Alice_Config_File
     (Self : in out Object; File : String)
      return Alice.VCS.Profile.Result.Object'Class;

   overriding
   function Get_Member_Repository
     (Self    : in out Object;
      Profile : Alice.VCS.Profile.Object'Class;
      Name    : String) return Alice.Result.Object'Class;

   overriding
   function Create_Member_Repository
     (Self        : in out Object;
      Profile     : Alice.VCS.Profile.Object'Class;
      Name        : String;
      Description : String) return Alice.Result.Object'Class;

   overriding
   function Create_Member_Repository_From_Template
     (Self        : in out Object;
      Profile     : Alice.VCS.Profile.Object'Class;
      Template    : String;
      Name        : String;
      Description : String) return Alice.Result.Object'Class;

   overriding
   function Get_User
     (Self : in out Object; Name : String) return Alice.Result.Object'Class;

   overriding
   function Get_User_Repository
     (Self : in out Object; Token : String) return Alice.Result.Object'Class;

end Alice.VCS.Service.GitHub;
