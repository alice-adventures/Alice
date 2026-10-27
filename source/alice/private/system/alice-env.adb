-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.Config;
with Alice.Context;
with Alice.IFace.VCS.Repository;
with Alice.Std;
with Alice.VCS.Repository;

package body Alice.Env is

   use all type Ada.Strings.Text_Buffers.Text_Buffer_Count;

   -----------------------------
   -- CWD_Is_Alice_Repository --
   -----------------------------

   function CWD_Is_Alice_Repository return Boolean is
      Context : constant Alice.Context.Object_Access := Alice.Std.Get_Context;
   begin
      if Alice.VCS.Repository.CWD_Is_A_Git_Repository then
         Context.Log.Debug ("Current working directory is a git repository");

         Repository : constant Alice.IFace.VCS.Repository.Object_Access :=
           Alice.VCS.Repository.Get_Repository_From_CWD.Repository;

         if Repository.all.Is_Clone_Of
              (Alice.Config.Repository.HTTPS (Alice.Config.Repository.Alice))
           or else Repository.all.Is_Clone_Of
                     (Alice.Config.Repository.SSH
                        (Alice.Config.Repository.Alice))
         then
            Context.Log.Debug
              ("Current working directory is an Alice repository");
            return True;
         else
            Context.Log.Debug
              ("Current working directory is a git repository, but NOT an "
               & "Alice repository. Expected origin: "
               & Alice.Config.Repository.HTTPS (Alice.Config.Repository.Alice)
               & " or "
               & Alice.Config.Repository.SSH (Alice.Config.Repository.Alice));
         end if;
      else
         Context.Log.Debug
           ("Current working directory is NOT a git repository");
      end if;
      return False;
   end CWD_Is_Alice_Repository;

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
