-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Protocols;
with Repositories;

package Alice_Repository is

   package Prot renames Protocols;

   type Alice_Repository_Type is
     (Main, Index, Log, Project_Euler, Project_Euler_Share);

   function Base_Name (Repository : Alice_Repository_Type) return String;
   --  Return the base name of an Alice repository.
   --  e.g. Base_Name (Main) --> "Alice"

   function Name (Repository : Alice_Repository_Type) return String;
   --  Return the name of an Alice repository.
   --  e.g. Name (Index) --> "alice-adventures/alice-index"

   function Name (User, Repository : String) return String renames
     Repositories.Name;
   --  Return the name of the Repository of the User.
   --  e.g. Name ("bob", "foo") --> "user/foo"

   function Host return String;
   --  Return the name of the remote Host.

   function URL
     (Repository : Alice_Repository_Type; Protocol : Prot.Name := Prot.git)
      return String;
   --  Return the URL of an Alice repository using the given Protocol.
   --  e.g. URL (git, Log) --> "git@github.com:alice-adventures/alice-log"

   function URL
     (Repository : String; Protocol : Prot.Name := Prot.git) return String;
   --  Return the URL of the Repository using the given Protocol.
   --  e.g. URL ("bob/foo", https) --> "https://github.com/bob/foo"

end Alice_Repository;
