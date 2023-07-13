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

   OS_Cmd_Instance : Cmd_Type;

   ----------
   -- Init --
   ----------

   function Init
     (Cmd : in out Cmd_Type; Report_Error : Boolean := True) return Boolean
   is
   begin
      if OS_Cmd_Instance.OS_Path = null then
         OS_Cmd_Instance.OS_Path :=
           GNAT.OS_Lib.Locate_Exec_On_Path (To_String (OS_Cmd_Name));

         if OS_Cmd_Instance.OS_Path /= null then
            Log.Debug
              ("found '" & To_String (OS_Cmd_Name) & "' at '" &
               OS_Cmd_Instance.OS_Path.all & "'");
         elsif Report_Error then
            Alice_Cmd.Exit_Status := 1;
            Log.Error
              ("'" & To_String (OS_Cmd_Name) & "' cannot be found in PATH");
            return False;
         end if;
      end if;

      Cmd.OS_Path := OS_Cmd_Instance.OS_Path;
      return (Cmd.OS_Path /= null);
   end Init;

   ----------
   -- Path --
   ----------

   function Path (Cmd : Cmd_Type) return String is (Cmd.OS_Path.all);

   ---------
   -- Run --
   ---------

   function Run (Cmd : Cmd_Type; Args : String) return Run_Output_Type is
      Arg_List   : GNAT.OS_Lib.Argument_List_Access;
      Run_Output : Run_Output_Type;
   begin
      Arg_List := GNAT.OS_Lib.Argument_String_To_List (Args);

      --  Debug all arguments:
      --  for Arg of Arg_List.all loop
      --     Log.Debug ("Arg : " & Arg.all);
      --  end loop;

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

   procedure Clean (Cmd : in out Cmd_Type; Run_Output : out Run_Output_Type) is
      Success : Boolean;
   begin
      if Run_Output.Temp_File /= null then
         GNAT.OS_Lib.Delete_File (Run_Output.Temp_File.all, Success);
      end if;
      GNAT.OS_Lib.Free (Run_Output.Temp_File);
   end Clean;

end OS_Cmd;
