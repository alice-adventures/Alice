-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Text_IO;
with GNAT.Source_Info;

with Alice.Result;
with Alice.VCS.Profile.Result;
with Alice.VCS.Service.GitHub;

package body Test.VCS_Service is

   GitHub_Token_Test_File : constant String :=
     "source/alice_cli/test/file/github-token";

   procedure Run (Ctx : Alice.Context.Object_Access) is
      GitHub : Alice.VCS.Service.GitHub.Object;
   begin
      Test.Title (GNAT.Source_Info.Enclosing_Entity);

      if Ada.Directories.Exists (GitHub_Token_Test_File) then
         Token_File : Ada.Text_IO.File_Type;
         Ada.Text_IO.Open
           (Token_File, Ada.Text_IO.In_File, GitHub_Token_Test_File);
         Token : constant String := Ada.Text_IO.Get_Line (Token_File);
         Ada.Text_IO.Close (Token_File);

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
           ("Token file not found: provide a valid GitHub token in file '"
            & GitHub_Token_Test_File & "' to run this test");
      end if;
   end Run;

end Test.VCS_Service;
