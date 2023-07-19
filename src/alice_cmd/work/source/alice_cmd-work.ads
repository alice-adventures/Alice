-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with AAA.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Alice_Cmd.Work is

   type Source_Name is (Project_Euler);

   type Source_Type is record
      Name  : Unbounded_String;
      Tag   : Unbounded_String;
      URL   : Unbounded_String;
      About : AAA.Strings.Vector;
   end record;

   --!pp off
   pragma Style_Checks (off);

   Available_Sources : constant array (Source_Name) of Source_Type :=
     [Project_Euler =>
       (Name  => To_Unbounded_String ("Project Euler"),
        Tag   => To_Unbounded_String ("peuler"),
        URL   => To_Unbounded_String ("https://projecteuler.net"),
        About =>
          AAA.Strings.Empty_Vector
          .Append ("Project Euler is a series of challenging mathematical/computer programming problems that will require more than just mathematical insights to solve.")
          .Append ("Although mathematics will help you arrive at elegant and efficient methods, the use of a computer and programming skills will be required to solve most problems."))];

   pragma Style_Checks (on);
   --!pp on

   function Is_Valid_Source_Tag (Tag : Unbounded_String) return Boolean;

   function Is_Valid_Source_Tag (Tag : String) return Boolean is
     (Is_Valid_Source_Tag (To_Unbounded_String (Tag)));

end Alice_Cmd.Work;
