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
with Alice.VCS.Profile;
with Alice.VCS.Service;

with Test.Fixtures;

package body Test.VCS.Profile is

   use all type Alice.Result.Status_Type;
   use all type Alice.VCS.Profile.Object;
   use all type Alice.VCS.Service.Name.Enum;

   package Load_From_File is
      procedure Succeeds (Context : Alice.Context.Object_Access);

      procedure With_Invalid_Profile_Fails
        (Context : Alice.Context.Object_Access);

      procedure With_Invalid_File_Fails
        (Context : Alice.Context.Object_Access);
   end Load_From_File;

   package body Load_From_File is

      -----------------------------
      -- Succeeds --
      -----------------------------

      procedure Succeeds (Context : Alice.Context.Object_Access) is
         Profile : Alice.VCS.Profile.Object;
         P       : constant Alice.VCS.Profile.Object_Access :=
           Alice.VCS.Profile.Create_Profile
             (Service    => Alice.VCS.Service.Name.GitHub,
              Token      => Alice.UStr ("invalid-token"),
              Login      => Alice.UStr ("test-user"),
              Avatar_URL => Alice.UStr ("https://example.com/avatar.png"),
              Name       => Alice.UStr ("Test User"),
              Email      => Alice.UStr ("test@example.com"),
              SPDX_Id    => Alice.UStr ("MIT"));
         Result  : constant Alice.Result.Object'Class :=
           Profile.Load_From_File (Test.Fixtures.GitHub.Profile_File);
      begin
         Subtitle (GNAT.Source_Info.Enclosing_Entity);

         case Result.Status is
            when Alice.Result.Success =>
               Context.Log.Info
                 (Alice.Env.New_Line & "Profile => " & Profile'Image);
               if Profile = P.all then
                  Pass;
               else
                  Error (Alice.Result.Error);
               end if;

            when Alice.Result.Error =>
               Error (Alice.Result.Error);
         end case;
      end Succeeds;

      -----------------------------------------------
      -- With_Invalid_Profile_Fails --
      -----------------------------------------------

      procedure With_Invalid_Profile_Fails
        (Context : Alice.Context.Object_Access)
      is
         Profile : Alice.VCS.Profile.Object;
      begin
         Subtitle (GNAT.Source_Info.Enclosing_Entity);

         Result_1 : constant Alice.Result.Object'Class :=
           Profile.Load_From_File
             (Test.Fixtures.GitHub.Invalid_Profile_Service_File);
         case Result_1.Status is
            when Alice.Result.Success =>
               Error (Alice.Result.Success);

            when Alice.Result.Error =>
               Context.Log.Info
                 ("Error as expected: " & Alice.Str (Result_1.Message));
         end case;

         Result_2 : constant Alice.Result.Object'Class :=
           Profile.Load_From_File
             (Test.Fixtures.GitHub.Invalid_Profile_Missing_Keys);
         case Result_2.Status is
            when Alice.Result.Success =>
               Error (Alice.Result.Success);

            when Alice.Result.Error =>
               Context.Log.Info
                 ("Error as expected: " & Alice.Str (Result_2.Message));
         end case;

         if Result_1.Status = Alice.Result.Error
           and then Result_2.Status = Alice.Result.Error
         then
            Pass;
         else
            Error (Alice.Result.Error);
         end if;

      end With_Invalid_Profile_Fails;

      --------------------------------------------
      -- With_Invalid_File_Fails --
      --------------------------------------------

      procedure With_Invalid_File_Fails (Context : Alice.Context.Object_Access)
      is
         Profile : Alice.VCS.Profile.Object;
         Result  : constant Alice.Result.Object'Class :=
           Profile.Load_From_File ("non-existent-file.toml");
      begin
         Subtitle (GNAT.Source_Info.Enclosing_Entity);

         case Result.Status is
            when Alice.Result.Success =>
               Error (Alice.Result.Success);

            when Alice.Result.Error =>
               Context.Log.Info
                 ("Error as expected: " & Alice.Str (Result.Message));
               Pass;
         end case;
      end With_Invalid_File_Fails;

   end Load_From_File;

   -------------------
   -- Run_All_Tests --
   -------------------

   procedure Run_All_Tests (Context : Alice.Context.Object_Access) is
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);

      Load_From_File.Succeeds (Context);
      Load_From_File.With_Invalid_File_Fails (Context);
      Load_From_File.With_Invalid_Profile_Fails (Context);
   end Run_All_Tests;

end Test.VCS.Profile;
