-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Test.VCS is

   GitHub_Token_File : constant String :=
     "source/alice_cli/test/file/github-token";

   GitHub_Profile_File : constant String :=
     "source/alice_cli/test/file/github-profile.toml";

   GitHub_Invalid_Profile_Service_File : constant String :=
     "source/alice_cli/test/file/github-profile-invalid-service.toml";

   GitHub_Invalid_Profile_Missing_Keys : constant String :=
     "source/alice_cli/test/file/github-profile-missing-keys.toml";

   function Get_Github_Token_From_Test_File return String;
   --  Reads the GitHub token from a test file.

end Test.VCS;
