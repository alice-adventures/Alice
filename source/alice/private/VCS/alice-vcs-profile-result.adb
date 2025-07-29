-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package provides the Result type for operations related to VCS
--  profiles in the Alice application. It defines a tagged record that
--  encapsulates the result of operations, including success and error cases.

with Alice.Std;
with Ada.Unchecked_Deallocation;

package body Alice.VCS.Profile.Result is

   -------------
   -- Success --
   -------------

   function Success
     (Profile : Alice.VCS.Profile.Object_Access) return Object'Class is
   begin
      return Result : Alice.VCS.Profile.Result.Object (Alice.Result.Success) do
         Result.Profile := Profile;
      end return;
   end Success;

   -----------
   -- Error --
   -----------

   function Error
     (Level   : Alice.Result.Error_Level;
      Message : Alice.UString;
      Hint    : Alice.Hint.Id := Alice.Hint.None) return Object'Class is
   begin
      return Result : Alice.VCS.Profile.Result.Object (Alice.Result.Error) do
         Result.Level := Level;
         Result.Message := Message;
         Result.Hint := Hint;
      end return;
   end Error;

   -----------------
   -- Get_Profile --
   -----------------

   function Get_Profile
     (Self : in out Object) return Alice.VCS.Profile.Object_Access is
   begin
      case Self.Status is
         when Alice.Result.Success =>
            return Self.Profile;

         when Alice.Result.Error =>
            return null;
      end case;
   end Get_Profile;

   ----------
   -- Free --
   ----------

   procedure Free is new
     Ada.Unchecked_Deallocation
       (Alice.VCS.Profile.Object,
        Alice.VCS.Profile.Object_Access);

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Self : in out Object) is
   begin
      Alice.Std.Get_OS_Context.Log.Trace_Begin;
      case Self.Status is
         when Alice.Result.Success =>
            Free (Self.Profile);

         when Alice.Result.Error =>
            null;
      end case;
   end Finalize;

end Alice.VCS.Profile.Result;
