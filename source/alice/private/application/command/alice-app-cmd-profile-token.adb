-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.Config;
with Alice.Hint;
with Alice.VCS.Profile;
with Alice.VCS.Profile.Result;
with Alice.VCS.Service.GitHub;

package body Alice.App.Cmd.Profile.Token is

   use all type Alice.Result.Status_Type;

   ---------
   -- Run --
   ---------

   overriding
   function Run
     (Self : in out Object; Token : String) return Alice.Result.Object'Class
   is
      GitHub_Service : Alice.VCS.Service.GitHub.Object;
   begin
      Self.Context.Log.Trace_Begin;

      Self.Context.Prog.Start ("Setting up profile from GitHub token");
      Profile_Result : Alice.VCS.Profile.Result.Object'Class :=
        GitHub_Service.Get_Member_Profile_From_Token (Token);

      if Profile_Result.Status = Alice.Result.Error then
         Self.Context.Prog.Fail;
         Profile_Result.Hint := Alice.Hint.Invalid_GitHub_Token;
         Self.Context.Log.Trace_Return (Profile_Result'Image);
         return Profile_Result;
      end if;

      Self.Context.Prog.Done;

      Profile : constant Alice.VCS.Profile.Object_Access :=
        Profile_Result.Get_Profile;
      Profile.Set_SPDX_Id (Alice.VCS.Profile.Default_SPDX_Id);

      Save_Result : Alice.Result.Object'Class :=
        Profile.Save_To_File (Alice.Config.Local.Profile);

      if Save_Result.Status = Alice.Result.Success then
         Self.Context.Log.Info ("Profile saved successfully");
         Self.Context.Log.Trace_Return (Save_Result'Image);
         return Save_Result;
      else
         Save_Result.Hint := Alice.Hint.File_Write_Error;
         Self.Context.Log.Trace_Return (Save_Result'Image);
         return Save_Result;
      end if;
   end Run;

end Alice.App.Cmd.Profile.Token;
