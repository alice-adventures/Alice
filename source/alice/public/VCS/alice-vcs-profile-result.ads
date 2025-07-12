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

with Alice.Result;

package Alice.VCS.Profile.Result is

   type Object (Status : Alice.Result.Status_Type) is
     new Alice.Result.Object with private;

   function Create_Object
     (Status        : Alice.Result.Status_Type;
      Profile       : Alice.VCS.Profile.Object_Access := null;
      Error_Level   : Alice.Result.Error_Level := Alice.Result.External;
      Error_Message : Alice.UString := Alice.Null_UString)
     --  #FIXME - Should include Level and Message parameters for detailed
     --  error reporting?
      return Object'Class;
   --  Create function constructs a new Object of type
   --  Alice.VCS.Profile.Result.Object. It takes a status indicating the
   --  result of the operation and an optional Profile object. If the
   --  operation was successful, the Profile parameter should contain a valid
   --  reference to the VCS profile object. If the operation failed, the
   --  Profile member is null. This function allows the caller to create a
   --  result object that encapsulates the outcome of the operation, making it
   --  easy to handle success and error cases in a consistent manner.

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
