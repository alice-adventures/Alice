-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.Config;
with Alice.Hint;
with Alice.Result;
with Alice.VCS.Profile;

package body Alice.Use_Case.Query.Profile.Show is

   use all type Alice.Result.Status_Type;

   ---------
   -- Run --
   ---------

   overriding
   function Run
     (Self : in out Object; Args : String := "")
      return Alice.IFace.Use_Case.Query.Result.Object'Class
   is
      Profile     : Alice.VCS.Profile.Object;
      Load_Result : constant Alice.Result.Object'Class :=
        Profile.Load_From_File (Alice.Config.Local.Profile);
   begin
      return
         Result : constant Alice.IFace.Use_Case.Query.Result.Object :=
           (if Load_Result.Status = Alice.Result.Success
            then
              Alice.IFace.Use_Case.Query.Result.Success
                (Alice.UStr (Profile.To_String))
            else
              Alice.IFace.Use_Case.Query.Result.Error
                (Alice.Result.Domain,
                 Load_Result.Message,
                 Alice.Hint.Profile_Not_Found));
   end Run;

end Alice.Use_Case.Query.Profile.Show;
