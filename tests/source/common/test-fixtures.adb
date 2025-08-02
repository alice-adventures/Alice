-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO;

package body Test.Fixtures is

   package body GitHub is

      ------------------------------
      -- Get_Token_From_Test_File --
      ------------------------------

      function Get_Token_From_Test_File return String is
      begin
         if Ada.Directories.Exists (Token_File) then
            Token_FD : Ada.Text_IO.File_Type;
            Token_FD.Open (Ada.Text_IO.In_File, Token_File);
            Token : constant String := Token_FD.Get_Line;
            Token_FD.Close;
            return Token;
         else
            return "";
         end if;
      end Get_Token_From_Test_File;

   end GitHub;

end Test.Fixtures;
