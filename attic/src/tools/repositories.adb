-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package body Repositories is

   ----------
   -- Name --
   ----------

   function Name (User, Repository : String) return String is
     (User & "/" & Repository);
   pragma Inline (Name);

   ---------
   -- URL --
   ---------

   function URL
     (Protocol : Prot.Name; Host : String; Repository : String) return String
   is
   begin
      case Protocol is
         when Prot.git =>
            return "git@" & Host & ":" & Repository;
         when Prot.https =>
            return "https://" & Host & "/" & Repository;
      end case;
   end URL;

   ---------
   -- URL --
   ---------

   function URL
     (Protocol : Prot.Name; Host : String; User, Repository : String)
      return String
   is
   begin
      case Protocol is
         when Prot.git =>
            return "git@" & Host & ":" & Name (User, Repository);
         when Prot.https =>
            return "https://" & Host & "/" & Name (User, Repository);
      end case;
   end URL;

end Repositories;
