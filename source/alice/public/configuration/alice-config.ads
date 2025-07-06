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

package Alice.Config is

   package Local is
      --  Local configuration settings for the Alice application. This package
      --  defines the directory and profile file used for local configuration.

      Directory : constant String := "config";
      Profile   : constant String := "profile.toml";

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

      package Platform is
         --  Platform-specific repository settings for the Alice application.

         Project_Euler       : constant String := "project_euler";
         Project_Euler_Share : constant String := "project_euler-share";

      end Platform;
   end Repository;

end Alice.Config;
