-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.App.Cmd.Profile.Token;
with Alice.Config;
with Alice.Hint;
with Alice.VCS.Profile;

package body Alice.App.Cmd.Profile.Update is

   use all type Alice.Result.Status_Type;

   ---------
   -- Run --
   ---------

   overriding
   function Run
     (Self : in out Object; Args : String) return Alice.Result.Object'Class is
   begin
      Self.Context.Log.Trace_Begin;

      Profile : Alice.VCS.Profile.Object;
      Result : Alice.Result.Object'Class :=
        Profile.Load_From_File (Alice.Config.Local.Profile);

      if Result.Status = Alice.Result.Error then
         Result.Hint := Alice.Hint.Profile_Not_Found;
         Self.Context.Log.Trace_Return (Result'Image);
         return Result;
      end if;

      Member_Token : constant String := Profile.Get_Token;
      Member_SPDX_Id : constant String := Profile.Get_SPDX_Id;

      App_Cmd_Profile_Token : Alice.App.Cmd.Profile.Token.Object;
      Token_Result : Alice.Result.Object'Class :=
        App_Cmd_Profile_Token.Run (Member_Token);

      if Token_Result.Status = Alice.Result.Error then
         Token_Result.Hint := Alice.Hint.Invalid_GitHub_Token;
         Self.Context.Log.Trace_Return (Token_Result'Image);
         return Token_Result;
      end if;

      --  Keep the existing SPDX ID

      Profile.Set_SPDX_Id (Member_SPDX_Id);
      Save_Result : Alice.Result.Object'Class :=
        Profile.Save_To_File (Alice.Config.Local.Profile);

      if Save_Result.Status = Alice.Result.Success then
         Self.Context.Log.Info ("Profile updated successfully");
         Self.Context.Log.Trace_Return (Save_Result'Image);
         return Save_Result;
      else
         Save_Result.Hint := Alice.Hint.File_Write_Error;
         Self.Context.Log.Trace_Return (Save_Result'Image);
         return Save_Result;
      end if;
   end Run;

end Alice.App.Cmd.Profile.Update;
