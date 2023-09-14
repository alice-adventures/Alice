-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Alice_Configuration is

   package Local is
      Config_Directory : constant String := "config";
      Profile          : constant String := "profile.toml";
   end Local;

   package Alire is
      Index_Name : constant String := "alice";
      Index_URL  : constant String :=
        "git+https://github.com/alice-adventures/alice-index";
   end Alire;

   package Repositories is
      Host_Name : constant String := "github.com";
      Alice_Org : constant String := "alice-adventures";

      Main  : constant String := "Alice";
      Index : constant String := "alice-index";
      Log   : constant String := "alice-log";
      Test  : constant String := "alice-test";

      package Sources is
         Project_Euler       : constant String := "project_euler";
         Project_Euler_Share : constant String := "project_euler-share";
      end Sources;
   end Repositories;

end Alice_Configuration;
