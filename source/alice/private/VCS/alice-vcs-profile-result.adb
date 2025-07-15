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

   ------------
   -- Create --
   ------------

   function Create_Object
     (Status        : Alice.Result.Status_Type;
      Profile       : Alice.VCS.Profile.Object_Access := null;
      Error_Level   : Alice.Result.Error_Level := Alice.Result.External;
      Error_Message : Alice.UString := Alice.Null_UString) return Object'Class
   is
   begin
      return Result : Alice.VCS.Profile.Result.Object (Status) do
         case Status is
            when Alice.Result.Success =>
               Result.Profile := Profile;

            when Alice.Result.Error =>
               Result.Level := Error_Level;
               Result.Message := Error_Message;
         end case;
      end return;
   end Create_Object;

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
