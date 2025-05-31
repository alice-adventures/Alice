-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Protocols;

package Repositories is

   package Prot renames Protocols;

   function Name (User, Repository : String) return String;
   --  Return the name of the Repository of the User.
   --  e.g. Name ("bob", "foo_bar") --> "bob/foo_bar"

   function URL
     (Protocol : Prot.Name; Host : String; Repository : String) return String;
   --  Return the URL of the Repository using the given Protocol and Host.
   --  e.g. URL ("bob/foo", https) --> "https://github.com/bob/foo"

   function URL
     (Protocol : Prot.Name; Host : String; User, Repository : String)
      return String;
   --  Return the URL of the Repository of the User using the given Protocol
   --  and Host.
   --  e.g. URL ("bob", "foo", https) --> "https://github.com/bob/foo"

end Repositories;
