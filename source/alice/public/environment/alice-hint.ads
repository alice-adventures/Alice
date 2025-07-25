-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  Hint messages to provide additional context or suggestions for resolving
--  common issues encountered by users. These hints can be used to guide users
--  in troubleshooting problems or understanding the expected usage of the
--  application. They are not intended to be exhaustive but rather to provide
--  helpful suggestions for common scenarios.

package Alice.Hint is

   type Id is
     (None,
      Invalid_GitHub_Token,
      Invalid_SPDX_Id,
      File_Write_Error,
      File_Read_Error,
      File_Not_Found,
      File_Permission,
      Invalid_Args);

   type String_Access is not null access constant String;

   --!pp off
   pragma Style_Checks (off);
   Message : constant array (Id) of String_Access :=
   [
      None                 => new String'(""),
      Invalid_GitHub_Token => new String'("Invalid GitHub token provided"),
      Invalid_SPDX_Id      => new String'("Invalid SPDX license Id provided"),
      File_Write_Error     => new String'("Error writing to file, check permissions"),
      File_Read_Error      => new String'("Error reading from file, check permissions"),
      File_Not_Found       => new String'("File not found, check the file path"),
      File_Permission      => new String'("Insufficient permissions to access the file"),
      Invalid_Args         => new String'("Invalid number of arguments provided")
   ];
   pragma Style_Checks (on);
   --!pp on

   function Get_Message (Hint_Id : Id) return Alice.UString
   is (Alice.UStr (Message (Hint_Id).all));
end Alice.Hint;
