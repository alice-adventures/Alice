-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  Configuration settings for the Alice application. This package should
--  include paths, URLs, and other configuration parameters needed by the
--  application.

with Ada.Directories;

package Alice.Config is

   package Local is
      --  Local configuration settings for the Alice application. This package
      --  defines the directory, configuration file and profile used for local
      --  configuration.

      Directory : constant String := "config";
      --  The directory where the Alice configuration files are stored. This
      --  directory is typically used to hold configuration files, profiles,
      --  and other related data for the Alice application. It is expected to
      --  be located in the Alice root directory. It is used to organize the
      --  configuration files and make it easier to manage the settings for
      --  the Alice application.

      Config_File : constant String :=
        Ada.Directories.Compose (Directory, "alice_config.toml");
      --  The file where the Alice configuration is stored. This file contains
      --  various settings and parameters that define the behavior of the
      --  Alice application, such as paths, user preferences, and other
      --  configuration options. It is typically located in the user's home
      --  directory or a specific configuration directory. The file is
      --  expected to be in TOML format.

      Profile : constant String :=
        Ada.Directories.Compose (Directory, "profile.toml");
      --  The file where the Alice member profile is stored. This file
      --  contains user-specific information such as name, email, and other
      --  profile details. It is typically used to personalize the member's
      --  experience within the Alice application. The profile file is
      --  expected to be in TOML format and is usually located in the same
      --  directory as the main configuration file. This allows the
      --  application to easily access and manage member profiles.

   end Local;

   package Alire is
      --  Alire configuration settings for the Alice application. This package
      --  defines the index name and URL used for Alire package management.

      Index_Name : constant String := "alice";
      Index_URL  : constant String :=
        "git+https://github.com/alice-adventures/alice-index";

   end Alire;

   package Repository is
      --  Repository configuration settings for the Alice application. This
      --  package defines the host name, organization, and repository names
      --  used for the Alice repositories on GitHub.

      Host_Name : constant String := "github.com";
      Alice_Org : constant String := "alice-adventures";

      Main  : constant String := "Alice";
      Index : constant String := "alice-index";
      Log   : constant String := "alice-log";
      Test  : constant String := "alice-test";

      --  #REVIEW - Each platforms should be defined in a separate file.
      package Platform is
         --  Platform-specific repository settings for the Alice application.

         Project_Euler       : constant String := "project_euler";
         Project_Euler_Share : constant String := "project_euler-share";

      end Platform;
   end Repository;

end Alice.Config;
