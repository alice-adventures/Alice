-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Simple_Logging;

package body OS_Cmd is

   package Log renames Simple_Logging;

   ----------
   -- Init --
   ----------

   procedure Init (Cmd : in out OS_Cmd_Type; Cmd_Name : String) is
   begin
      Cmd.OS_Path := GNAT.OS_Lib.Locate_Exec_On_Path (Cmd_Name);
      if Cmd.OS_Path = null then
         Log.Error ("'" & Cmd_Name & "' cannot be found in PATH");
         GNAT.OS_Lib.OS_Exit (1);
      else
         Log.Debug ("found '" & Cmd_Name & "'' at '" & Cmd.OS_Path.all & "'");
      end if;
   end Init;

   ----------
   -- Path --
   ----------

   function Path (Cmd : OS_Cmd_Type) return String is (Cmd.OS_Path.all);

   ---------
   -- Run --
   ---------

   procedure Run
     (Cmd : OS_Cmd_Type; Args : String; Run_Output : out Run_Output_Type)
   is
      Arg_List : GNAT.OS_Lib.Argument_List_Access;
   begin
      Arg_List := GNAT.OS_Lib.Argument_String_To_List (Args);

      GNAT.OS_Lib.Create_Temp_Output_File
        (Run_Output.Temp_FD, Run_Output.Temp_File);

      GNAT.OS_Lib.Spawn
        (Cmd.OS_Path.all, Arg_List.all, Run_Output.Temp_FD,
         Run_Output.Return_Code);
      GNAT.OS_Lib.Free (Arg_List);
   end Run;

   -----------
   -- Clean --
   -----------

   procedure Clean (Cmd : OS_Cmd_Type; Run_Output : out Run_Output_Type) is
      Success : Boolean;
   begin
      GNAT.OS_Lib.Delete_File (Run_Output.Temp_File.all, Success);
   end Clean;

end OS_Cmd;
