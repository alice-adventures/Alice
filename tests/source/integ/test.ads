-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with AnsiAda;

with Alice.Result;

package Test is

   package ANSI renames AnsiAda;

   procedure Section (Section : String; Color : ANSI.Colors);
   --  Print a section header in the log. Use it to mark the beginning of a
   --  test or a section of the code. Typically, the section header is printed
   --  in a different color and with a special format to make it stand out in
   --  the log output.

   procedure Title (Title : String);
   --  Print a title in the log. Use it to mark the beginning of a test or a
   --  section of the code. Typically, the title is printed in a different
   --  color and with a special format to make it stand out in the log output.
   --
   --  Clients should call this procedure with the parameter
   --
   --     GNAT.Source_Info.Enclosing_Entity
   --
   --  to print the name of the current procedure or function.

   procedure Subtitle (Subtitle : String);
   --  Print a subtitle in the log. Use it to mark a subsection of a test or a
   --  section of the code.

   procedure Pass;
   --  Print a success message in the log. Use it to indicate that a test or a
   --  section of the code has completed successfully.

   procedure Fail (Message : String := "");
   --  Print a failure message in the log. Use it to indicate that a test or a
   --  section of the code has failed. This procedure should be called when an
   --  assertion fails or when an unexpected condition is encountered in the
   --  code.

   procedure Warning (Message : String);
   --  Print a warning message in the log. Use it to indicate that a test
   --  cannot be run or that a condition has been encountered that is not
   --  critical but should be noted.

   procedure Error (Status : Alice.Result.Status_Type);
   --  Print an error message in the log. Use it to indicate that an error has
   --  occurred in the test code itself.
   --
   --  If Status is Alice.Result.Success, it means that the test has
   --  erroneously succeeded when it was expected to fail. In this case, the
   --  message should indicate that the test has failed unexpectedly. And vice
   --  versa, if Status is Alice.Result.Error, it means that the test has
   --  erroneously failed when it was expected to succeed. In this case, the
   --  message should indicate that the test has succeeded unexpectedly.

end Test;
