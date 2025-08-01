-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with SPDX;

with Alice.Config;
with Alice.Hint;
with Alice.VCS.Profile;

package body Alice.Use_Case.Cmd.Profile.SPDX is

   use all type Alice.Result.Status_Type;

   ---------
   -- Run --
   ---------

   overriding
   function Run
     (Self : in out Object; SPDX_Id : String) return Alice.Result.Object'Class
   is
   begin
      Self.Context.Log.Trace_Begin;

      Profile : Alice.VCS.Profile.Object;
      Load_Result : constant Alice.Result.Object'Class :=
        Profile.Load_From_File (Alice.Config.Local.Profile);

      if Load_Result.Status = Alice.Result.Error then
         Self.Context.Log.Trace_Return (Load_Result'Image);
         return Load_Result;
      end if;

      Valid_SPDX_Id : constant Boolean :=
        Standard.SPDX.Valid (Standard.SPDX.Parse (SPDX_Id));
      if not Valid_SPDX_Id then
         Self.Context.Log.Warning
           ("Invalid SPDX ID '"
            & SPDX_Id
            & "'"
            & ", set to default '"
            & Alice.VCS.Profile.Default_SPDX_Id
            & "'"
            & " instead");
      end if;

      Real_SPDX_Id : constant String :=
        (if Valid_SPDX_Id then SPDX_Id else Alice.VCS.Profile.Default_SPDX_Id);

      Profile.Set_SPDX_Id (Real_SPDX_Id);

      Save_Result : Alice.Result.Object'Class :=
        Profile.Save_To_File (Alice.Config.Local.Profile);

      if Save_Result.Status = Alice.Result.Error then
         Save_Result.Hint := Alice.Hint.File_Write_Error;
      end if;
      return Save_Result;
   end Run;

end Alice.Use_Case.Cmd.Profile.SPDX;
