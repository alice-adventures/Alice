-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Test is

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
   --  section of the code. Typically, the subtitle is printed in a different
   --  color and with a special format to make it stand out in the log output.

   procedure Pass;
   --  Print a success message in the log. Use it to indicate that a test or a
   --  section of the code has completed successfully. Typically, the message
   --  is printed in a different color to make it stand out in the log output.

   procedure Fail (Message : String := "");
   --  Print a failure message in the log. Use it to indicate that a test or a
   --  section of the code has failed. Typically, the message is printed in a
   --  different color to make it stand out in the log output. This procedure
   --  should be called when an assertion fails or when an unexpected
   --  condition is encountered in the code.

end Test;
