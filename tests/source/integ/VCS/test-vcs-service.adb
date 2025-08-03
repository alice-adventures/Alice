-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GNAT.Source_Info;

with Alice.Env;
with Alice.Result;
with Alice.VCS.Profile.Result;
with Alice.VCS.Service.GitHub;

with Test.Fixtures;

package body Test.VCS.Service is

   package Get_Member_Profile_From_Token is
      procedure Succeeds (Context : Alice.Context.Object_Access);

      procedure With_Invalid_Token_Fails
        (Context : Alice.Context.Object_Access);
   end Get_Member_Profile_From_Token;

   package body Get_Member_Profile_From_Token is

      --------------
      -- Succeeds --
      --------------

      procedure Succeeds (Context : Alice.Context.Object_Access) is
         GitHub : Alice.VCS.Service.GitHub.Object;
         Token  : constant String :=
           Test.Fixtures.GitHub.Get_Token_From_Test_File;
      begin
         Subtitle (GNAT.Source_Info.Enclosing_Entity);

         if Token'Length > 0 then
            Result : Alice.VCS.Profile.Result.Object'Class :=
              GitHub.Get_Member_Profile_From_Token (Token);

            case Result.Status is
               when Alice.Result.Success =>
                  Profile : constant Alice.VCS.Profile.Object_Access :=
                    Result.Get_Profile;
                  Context.Log.Info
                    (Alice.Env.New_Line & "Profile => " & Profile.all'Image);
                  Pass;

               when Alice.Result.Error =>
                  Context.Log.Warning (Alice.Str (Result.Message));
                  Fail ("Failed to get member profile from token");
            end case;
         else
            Warning
              ("Token file not found or is empty:"
               & " provide a valid GitHub token in file '"
               & Test.Fixtures.GitHub.Token_File
               & "' to run this test");
         end if;
      end Succeeds;

      ------------------------------
      -- With_Invalid_Token_Fails --
      ------------------------------

      procedure With_Invalid_Token_Fails
        (Context : Alice.Context.Object_Access)
      is
         GitHub : Alice.VCS.Service.GitHub.Object;
      begin
         Subtitle (GNAT.Source_Info.Enclosing_Entity);

         Result : constant Alice.VCS.Profile.Result.Object'Class :=
           GitHub.Get_Member_Profile_From_Token ("invalid-token");

         case Result.Status is
            when Alice.Result.Success =>
               Fail ("Expected an error, but got success");

            when Alice.Result.Error =>
               Context.Log.Info
                 ("Received expected error: " & Alice.Str (Result.Message));
               Pass;
         end case;
      end With_Invalid_Token_Fails;

   end Get_Member_Profile_From_Token;

   -------------------
   -- Run_All_Tests --
   -------------------

   procedure Run_All_Tests (Context : Alice.Context.Object_Access) is
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);

      Get_Member_Profile_From_Token.Succeeds (Context);
      Get_Member_Profile_From_Token.With_Invalid_Token_Fails (Context);
   end Run_All_Tests;

end Test.VCS.Service;
