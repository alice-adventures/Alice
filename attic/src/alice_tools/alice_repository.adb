-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice_Configuration;

package body Alice_Repository is

   package Conf renames Alice_Configuration;

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name (Repository : Alice_Repository_Type) return String is
   begin
      case Repository is
         when Main =>
            return Conf.Repositories.Main;
         when Index =>
            return Conf.Repositories.Index;
         when Log =>
            return Conf.Repositories.Log;
         when Project_Euler =>
            return Conf.Repositories.Sources.Project_Euler;
         when Project_Euler_Share =>
            return Conf.Repositories.Sources.Project_Euler_Share;
      end case;
   end Base_Name;

   ----------
   -- Name --
   ----------

   function Name (Repository : Alice_Repository_Type) return String is
     (Repositories.Name (Conf.Repositories.Alice_Org, Base_Name (Repository)));

   ----------
   -- Host --
   ----------

   function Host return String is (Conf.Repositories.Host_Name);

   ---------
   -- URL --
   ---------

   function URL
     (Repository : Alice_Repository_Type; Protocol : Prot.Name := Prot.git)
      return String is
     (Repositories.URL
        (Protocol, Host, Conf.Repositories.Alice_Org, Base_Name (Repository)));

   ---------
   -- URL --
   ---------

   function URL
     (Repository : String; Protocol : Prot.Name := Prot.git) return String is
     (Repositories.URL (Protocol, Host, Repository));

   pragma Inline (Name);
   pragma Inline (Host);
   pragma Inline (URL);
end Alice_Repository;
