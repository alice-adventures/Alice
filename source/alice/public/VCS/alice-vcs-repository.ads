-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Text_Buffers;

with Alice.IFace.VCS.Repository;
with Alice.Result;

package Alice.VCS.Repository is

   use all type Alice.Result.Status_Type;

   package Result is
      type Object (Status : Alice.Result.Status_Type) is
        new Alice.Result.Object (Status)
      with record
         case Status is
            when Alice.Result.Success =>
               Repository : Alice.IFace.VCS.Repository.Object_Access;

            when Alice.Result.Error =>
               null;
         end case;
      end record
      with Put_Image => Put_Image_Object;

      procedure Put_Image_Object
        (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
         Self   : Object);
   end Result;

   function Get_Repository_From_CWD return Alice.VCS.Repository.Result.Object;
   --  Returns the repository object for the current working directory. The
   --  implementation should return the repository object if the current
   --  working directory is a valid repository (git, svn, hg,..), or an error
   --  if it is not. This is useful for operations that need to determine the
   --  repository context based on the current working directory, such as
   --  cloning, pulling, or pushing changes.

end Alice.VCS.Repository;
