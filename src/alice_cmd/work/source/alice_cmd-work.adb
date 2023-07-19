-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GNAT.AWK;

with Simple_Logging;

with OS_Cmd_Alr;

package body Alice_Cmd.Work is

   package Log renames Simple_Logging;

   ---------------------
   -- Is_Valid_Source --
   ---------------------

   function Is_Valid_Source (Id_Or_Tag : Unbounded_String) return Boolean is
   begin
      for Source of Available_Sources loop
         if Source.Id = Id_Or_Tag or else Source.Tag = Id_Or_Tag then
            return True;
         end if;
      end loop;

      return False;
   end Is_Valid_Source;

   ------------------------------
   -- Exists_Alice_Alire_Index --
   ------------------------------

   function Exists_Alice_Alire_Index return Boolean is
      Cmd_Alr      : OS_Cmd_Alr.Cmd_Type;
      Run_Output   : OS_Cmd_Alr.Run_Output_Type;
      Index_Exists : Boolean := False;

      procedure Alice_Match is
      begin
         if GNAT.AWK.Field (3) = Alice_Alire_Index_URL then
            Log.Debug ("AWK alice match");
            Index_Exists := True;
         else
            Abort_Execution ("Invalid Alice index URL");
         end if;
      end Alice_Match;
   begin
      if not Cmd_Alr.Init then
         return False;
      end if;
      Run_Output := Cmd_Alr.Run ("index --list");

      GNAT.AWK.Add_File (Run_Output.Temp_File.all);
      GNAT.AWK.Register
        (Field  => 2, Pattern => "alice",
         Action => Alice_Match'Unrestricted_Access);

      GNAT.AWK.Parse;
      GNAT.AWK.Close (GNAT.AWK.Default_Session.all);
      Run_Output.Clean;

      return Index_Exists;
   end Exists_Alice_Alire_Index;

   ---------------------------
   -- Add_Alice_Alire_Index --
   ---------------------------

   procedure Add_Alice_Alire_Index is
      Cmd_Alr    : OS_Cmd_Alr.Cmd_Type;
      Run_Output : OS_Cmd_Alr.Run_Output_Type;
   begin
      if not Cmd_Alr.Init then
         return;
      end if;
      Run_Output :=
        Cmd_Alr.Run ("index --add=" & Alice_Alire_Index_URL & " --name=alice");
      Run_Output.Clean;

      if Run_Output.Return_Code /= 0 then
         Abort_Execution ("Unknown error while adding Alice index");
      end if;
   end Add_Alice_Alire_Index;

   ------------------------------
   -- Ensure_Alice_Alire_Index --
   ------------------------------

   procedure Ensure_Alice_Alire_Index is
   begin
      if not Exists_Alice_Alire_Index then
         Add_Alice_Alire_Index;
      end if;

   end Ensure_Alice_Alire_Index;

end Alice_Cmd.Work;
