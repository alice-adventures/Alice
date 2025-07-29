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

with Alice.Hint;
with Alice.Result;

package Alice.VCS.Profile.Result is

   type Object (Status : Alice.Result.Status_Type) is
     new Alice.Result.Object with private;

   function Success
     (Profile : Alice.VCS.Profile.Object_Access) return Object'Class;
   --  Constructs a new Object  with a status of Success and the provided VCS
   --  profile. This function is used when an operation related to VCS
   --  profiles is successful and returns a valid profile object.

   function Error
     (Level   : Alice.Result.Error_Level;
      Message : Alice.UString;
      Hint    : Alice.Hint.Id := Alice.Hint.None) return Object'Class;
   --  Constructs a new Object with a status of Error, providing the error
   --  level, message, and an optional hint. This function is used when an
   --  operation related to VCS profiles fails, allowing the caller to handle
   --  the error appropriately.

   function Get_Profile
     (Self : in out Object) return Alice.VCS.Profile.Object_Access
   with Inline;
   --  Get_Profile retrieves the VCS profile object from the result object. If
   --  the operation was successful, it returns a valid reference to the
   --  profile. If the operation failed, it returns null. This function is
   --  useful for accessing the profile information after an operation has
   --  been performed, allowing the caller to check the result and handle it

private

   type Object (Status : Alice.Result.Status_Type) is
     new Alice.Result.Object (Status)
   with record
      case Status is
         when Alice.Result.Success =>
            Profile : Alice.VCS.Profile.Object_Access;

         when Alice.Result.Error =>
            null;
      end case;
   end record;
   --  Object is a tagged record that encapsulates the result of operations
   --  related to VCS profiles. It contains the status of the operation and,
   --  in case of success, a reference to the VCS profile object. If the
   --  operation fails, it contains no additional information. This allows the
   --  caller to check the status and handle the result accordingly.

   overriding
   procedure Finalize (Self : in out Object);
   --  Finalize procedure ensures that the Profile field is deallocated
   --  properly when the Object is finalized to avoid memory leaks. If the
   --  Status is Alice.Result.Success, it deallocates the Profile object. If
   --  the Status is Alice.Result.Error, it does nothing as there is no
   --  Profile to deallocate.

end Alice.VCS.Profile.Result;
