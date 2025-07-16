-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Text_IO;

package body Test.VCS is

   -------------------------------------
   -- Get_Github_Token_From_Test_File --
   -------------------------------------

   function Get_Github_Token_From_Test_File return String is
   begin
      if Ada.Directories.Exists (GitHub_Token_Test_File) then
         Token_File : Ada.Text_IO.File_Type;
         Ada.Text_IO.Open
           (Token_File, Ada.Text_IO.In_File, GitHub_Token_Test_File);
         Token : constant String := Ada.Text_IO.Get_Line (Token_File);
         Ada.Text_IO.Close (Token_File);
         return Token;
      else
         return "";
      end if;
   end Get_Github_Token_From_Test_File;

end Test.VCS;
