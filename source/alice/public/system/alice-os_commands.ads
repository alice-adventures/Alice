-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Text_Buffers;

with Alice.IFace.OS_Cmd;

package Alice.OS_Commands is

   type Object is tagged record
      Alr  : Alice.IFace.OS_Cmd.Object_Access;
      Curl : Alice.IFace.OS_Cmd.Object_Access;
      Git  : Alice.IFace.OS_Cmd.Object_Access;
   end record
   with Put_Image => Put_Image_OS_Cmd;
   --  The OS_Commands record contains command objects for various OS commands
   --  used in the application, such as Alr, Git, and Curl. These commands are
   --  used to interact with the operating system and perform tasks such as
   --  building the project, managing dependencies, and executing external
   --  commands.

   type Object_Access is not null access all Object;

   procedure Put_Image_OS_Cmd
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Self   : Object);

end Alice.OS_Commands;
