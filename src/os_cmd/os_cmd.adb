-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice_Cmd;

with Simple_Logging;

package body OS_Cmd is

   package Log renames Simple_Logging;

   ----------
   -- Init --
   ----------

   function Init
     (Cmd          : in out OS_Cmd_Type; Cmd_Name : String;
      Report_Error :        Boolean := True) return Boolean
   is
      Success : Boolean;
   begin
      Cmd.OS_Path := GNAT.OS_Lib.Locate_Exec_On_Path (Cmd_Name);
      Success := (Cmd.OS_Path /= null);

      if Success then
         Log.Debug ("found '" & Cmd_Name & "' at '" & Cmd.OS_Path.all & "'");
      elsif Report_Error then
         Alice_Cmd.Exit_Status := 1;
         Log.Error ("'" & Cmd_Name & "' cannot be found in PATH");
      end if;

      return Success;
   end Init;

   ----------
   -- Path --
   ----------

   function Path (Cmd : OS_Cmd_Type) return String is (Cmd.OS_Path.all);

   ---------
   -- Run --
   ---------

   function Run (Cmd : OS_Cmd_Type; Args : String) return Run_Output_Type is
      Arg_List   : GNAT.OS_Lib.Argument_List_Access;
      Run_Output : Run_Output_Type;
   begin
      Arg_List := GNAT.OS_Lib.Argument_String_To_List (Args);

      GNAT.OS_Lib.Create_Temp_Output_File
        (Run_Output.Temp_FD, Run_Output.Temp_File);

      GNAT.OS_Lib.Spawn
        (Cmd.OS_Path.all, Arg_List.all, Run_Output.Temp_FD,
         Run_Output.Return_Code);

      GNAT.OS_Lib.Free (Arg_List);

      return Run_Output;
   end Run;

   -----------
   -- Clean --
   -----------

   procedure Clean (Cmd : in out OS_Cmd_Type; Run_Output : out Run_Output_Type)
   is
      Success : Boolean;
   begin
      if Run_Output.Temp_File /= null then
         GNAT.OS_Lib.Delete_File (Run_Output.Temp_File.all, Success);
      end if;
      GNAT.OS_Lib.Free (Run_Output.Temp_File);
      GNAT.OS_Lib.Free (Cmd.OS_Path);
   end Clean;

end OS_Cmd;
