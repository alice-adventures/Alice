-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  with Ada.Directories;

package body Alice.Env is

   use all type Ada.Strings.Text_Buffers.Text_Buffer_Count;

   -------------------------
   -- Is_Alice_Repository --
   -------------------------

   function Is_Alice_Repository (Report_Error : Boolean := True) return Boolean
   is (True);
   --  #TODO - Implement the actual check to determine if the current working
   --  directory is a clone of the Alice repository. This function should
   --  check for the presence of a specific file or directory that indicates
   --  the presence of the Alice repository. If Report_Error is True, it
   --  should raise Environment_Error if the check fails.

   -----------------------
   -- Is_Alice_Root_Dir --
   -----------------------

   function Is_Alice_Root_Dir (Report_Error : Boolean := True) return Boolean
   is (True);
   --  #TODO - Implement the actual check to determine if the current working
   --  directory is the root of the Alice repository. This function should
   --  check for the presence of specific files or directories that are only
   --  found at the root of the Alice repository. If Report_Error is True, it
   --  should raise Environment_Error if the check fails. This function should
   --  be called before calling Get_Alice_Root_Dir to ensure that the current
   --  directory is indeed the root of the Alice repository.

   ------------------------
   -- Get_Alice_Root_Dir --
   ------------------------

   function Get_Alice_Root_Dir return String
   is ("");
   --  #TODO - Implement the actual retrieval of the Alice root directory.
   --  This function should return the absolute path to the root directory of
   --  the Alice repository. If called before confirming that the current
   --  directory is the root of the Alice repository using Is_Alice_Root_Dir,
   --  it should return an empty String. This function is useful for obtaining
   --  the base directory of the Alice application, which can be used for
   --  various purposes such as loading configuration files, accessing
   --  resources, or performing operations that require knowledge of the
   --  repository structure.

   ---------------------
   -- Increase_Indent --
   ---------------------

   procedure Increase_Indent
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Amount : Ada.Strings.Text_Buffers.Text_Buffer_Count := 3) is
   begin
      Alice.Env.Current_Indent := Alice.Env.Current_Indent + Amount;
      Output.Increase_Indent (Alice.Env.Current_Indent);
   end Increase_Indent;

   ---------------------
   -- Decrease_Indent --
   ---------------------

   procedure Decrease_Indent
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Amount : Ada.Strings.Text_Buffers.Text_Buffer_Count := 3) is
   begin
      if Alice.Env.Current_Indent >= Amount then
         Output.Decrease_Indent (Amount);
         Alice.Env.Current_Indent := Alice.Env.Current_Indent - Amount;
      end if;
   end Decrease_Indent;

end Alice.Env;
