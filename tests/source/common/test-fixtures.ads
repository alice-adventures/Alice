-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;

package Test.Fixtures is

   --  This package contains test fixtures that can be used in the tests.
   --  Fixtures are reusable components that can be used to set up the test
   --  environment or to provide common functionality for the tests.

   --  The fixtures are organized in sub-packages, each subpackage containing
   --  a specific set of fixtures.

   Directory : constant String := "fixtures";

   package GitHub is
      Token_File : constant String :=
        Ada.Directories.Compose (Directory, "github-token");

      Profile_File : constant String :=
        Ada.Directories.Compose (Directory, "github-profile.toml");

      Invalid_Profile_Service_File : constant String :=
        Ada.Directories.Compose
          (Directory, "github-profile-invalid-service.toml");

      Invalid_Profile_Missing_Keys : constant String :=
        Ada.Directories.Compose
          (Directory, "github-profile-missing-keys.toml");

      function Get_Token_From_Test_File return String;
      --  Reads the GitHub token from a fixture file.
   end GitHub;

end Test.Fixtures;
