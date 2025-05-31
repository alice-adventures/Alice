-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice_Configuration;
with OS_Cmd_Alr;

with GNAT.AWK;
with Simple_Logging;

package body Alice_Alire is

   package Conf renames Alice_Configuration;
   package Log renames Simple_Logging;

   ------------------------
   -- Exists_Alice_Index --
   ------------------------

   function Exists_Alice_Index return Boolean is
      Cmd_Alr      : OS_Cmd_Alr.Cmd_Type;
      Run_Output   : OS_Cmd_Alr.Run_Output_Type;
      Index_Exists : Boolean := False;

      procedure Alice_Match is
      begin
         if GNAT.AWK.Field (3) = Conf.Alire.Index_URL then
            Log.Debug ("AWK alice match");
            Index_Exists := True;
         else
            raise Alire_Error with "Invalid Alice index URL";
         end if;
      end Alice_Match;

   begin
      Cmd_Alr.Init;
      Run_Output := Cmd_Alr.Run ("index --list");

      GNAT.AWK.Add_File (Run_Output.Temp_File.all);
      GNAT.AWK.Register
        (Field  => 2, Pattern => "alice",
         Action => Alice_Match'Unrestricted_Access);

      GNAT.AWK.Parse;
      GNAT.AWK.Close (GNAT.AWK.Default_Session.all);
      Run_Output.Clean;

      return Index_Exists;
   end Exists_Alice_Index;

   ---------------------
   -- Add_Alice_Index --
   ---------------------

   procedure Add_Alice_Index is
      Cmd_Alr    : OS_Cmd_Alr.Cmd_Type;
      Run_Output : OS_Cmd_Alr.Run_Output_Type;
   begin
      Cmd_Alr.Init;
      Run_Output :=
        Cmd_Alr.Run ("index --add=" & Conf.Alire.Index_URL & " --name=alice");
      Run_Output.Clean;

      if Run_Output.Return_Code /= 0 then
         raise Alire_Error with "Unknown error while adding Alice index";
      end if;

      Log.Detail ("Added Alice index in Alire");
   end Add_Alice_Index;

   ------------------------
   -- Ensure_Alice_Index --
   ------------------------

   procedure Ensure_Alice_Index is
   begin
      if not Exists_Alice_Index then
         Add_Alice_Index;
      end if;
   end Ensure_Alice_Index;

   --------------------
   -- Update_Indexes --
   --------------------

   procedure Update_Indexes is
      Cmd_Alr    : OS_Cmd_Alr.Cmd_Type;
      Run_Output : OS_Cmd_Alr.Run_Output_Type;
   begin
      Cmd_Alr.Init;
      Run_Output := Cmd_Alr.Run ("index --update-all");

      if Run_Output.Return_Code = 0 then
         Log.Detail ("Alire indexes updated");
      else
         raise Alire_Error with "Could not update Alire indexes";
      end if;
   end Update_Indexes;

   -----------------
   -- Build_Crate --
   -----------------

   procedure Build_Crate (Args : String := "") is
      Cmd_Alr : OS_Cmd_Alr.Cmd_Type;
   begin
      Cmd_Alr.Init;
      if Cmd_Alr.Run ("build " & Args) /= 0 then
         raise Alire_Error with "Could not build the current crate";
      end if;
   end Build_Crate;

end Alice_Alire;
