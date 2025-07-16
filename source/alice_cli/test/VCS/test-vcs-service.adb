-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GNAT.Source_Info;

with Alice.Result;
with Alice.VCS.Profile.Result;
with Alice.VCS.Service.GitHub;

package body Test.VCS.Service is

   ----------------------------------------------
   -- Get_Profile_From_Token_Ends_With_Success --
   ----------------------------------------------

   procedure Get_Profile_From_Token_Ends_With_Success
     (Ctx : Alice.Context.Object_Access)
   is
      GitHub : Alice.VCS.Service.GitHub.Object;
      Token  : constant String := Get_Github_Token_From_Test_File;
   begin
      Subtitle ("Get_Member_Profile_From_Token ends with success");

      if Token'Length > 0 then
         Result : Alice.VCS.Profile.Result.Object'Class :=
           GitHub.Get_Member_Profile_From_Token (Token);

         case Result.Status is
            when Alice.Result.Success =>
               Profile : constant Alice.VCS.Profile.Object_Access :=
                 Result.Get_Profile;
               Ctx.Log.Info (Profile.all'Image);
               Pass;

            when Alice.Result.Error =>
               Ctx.Log.Warning (Alice.Str (Result.Message));
               Fail ("Failed to get member profile from token");
         end case;
      else
         Fail
           ("Token file not found or is empty:"
            & " provide a valid GitHub token in file '"
            & GitHub_Token_Test_File
            & "' to run this test");
      end if;
   end Get_Profile_From_Token_Ends_With_Success;

   --------------------------------------------
   -- Get_Profile_From_Token_Ends_With_Error --
   --------------------------------------------

   procedure Get_Profile_From_Token_Ends_With_Error
     (Ctx : Alice.Context.Object_Access)
   is
      GitHub : Alice.VCS.Service.GitHub.Object;
   begin
      Subtitle ("Get_Member_Profile_From_Token ends with error");

      Result : constant Alice.VCS.Profile.Result.Object'Class :=
        GitHub.Get_Member_Profile_From_Token ("invalid-token");

      case Result.Status is
         when Alice.Result.Success =>
            Fail ("Expected an error, but got success");

         when Alice.Result.Error =>
            Ctx.Log.Info
              ("Received expected error: " & Alice.Str (Result.Message));
            Pass;
      end case;
   end Get_Profile_From_Token_Ends_With_Error;

   ---------
   -- Run --
   ---------

   procedure Run (Ctx : Alice.Context.Object_Access) is
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);

      Get_Profile_From_Token_Ends_With_Error (Ctx);
      Get_Profile_From_Token_Ends_With_Success (Ctx);
   end Run;

end Test.VCS.Service;
