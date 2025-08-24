-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.IFace.VCS.Repository;
with Alice.Result;

package Alice.VCS.Repository.Git is

   type Object is new Alice.IFace.VCS.Repository.Object with null record;

   overriding
   function CWD_Is_Clone_Of (Self : in out Object) return Boolean;

   overriding
   function Create
     (Self     : in out Object;
      Name     : String := "";
      URL      : String := "";
      Provider : String := "") return Alice.Result.Object'Class;

   overriding
   function Clone
     (Self : in out Object; Directory : String := ""; Branch : String := "")
      return Alice.Result.Object'Class;

   overriding
   function Switch
     (Self : in out Object; Branch : String) return Alice.Result.Object'Class;

   overriding
   function Pull (Self : in out Object) return Alice.Result.Object'Class;

   overriding
   function Push (Self : in out Object) return Alice.Result.Object'Class;

   overriding
   function Commit
     (Self : in out Object; Message : String) return Alice.Result.Object'Class;

   overriding
   function Get_Branches
     (Self : in out Object) return Alice.Result.Object'Class;

   overriding
   function Get_Current_Branch
     (Self : in out Object) return Alice.Result.Object'Class;

   overriding
   function Get_Status (Self : in out Object) return Alice.Result.Object'Class;

   overriding
   function Get_Log (Self : in out Object) return Alice.Result.Object'Class;

   overriding
   function Get_Name (Self : in out Object) return Alice.Result.Object'Class;

   overriding
   function Get_Description
     (Self : in out Object) return Alice.Result.Object'Class;

   overriding
   function Get_Owner (Self : in out Object) return Alice.Result.Object'Class;

   overriding
   function Get_URL (Self : in out Object) return Alice.Result.Object'Class;

   overriding
   function Get_Clone_URL
     (Self : in out Object) return Alice.Result.Object'Class;

   overriding
   function Get_Provider
     (Self : in out Object) return Alice.Result.Object'Class;

end Alice.VCS.Repository.Git;
