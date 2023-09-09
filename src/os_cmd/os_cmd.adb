-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Text_IO;

with Simple_Logging;

package body OS_Cmd is

   package Dir renames Ada.Directories;
   package Log renames Simple_Logging;

   use all type Dir.File_Size;
   use all type Log.Levels;

   OS_Cmd_Instance : Cmd_Type;

   ----------
   -- Init --
   ----------

   procedure Init (Cmd : in out Cmd_Type) is
   begin
      if not Cmd.Check then
         raise Command_Error
           with "Command " & To_String (OS_Cmd_Name) & " not found in PATH";
      end if;

      Cmd.OS_Path := OS_Cmd_Instance.OS_Path;
   end Init;

   -----------
   -- Check --
   -----------

   function Check (Cmd : in out Cmd_Type) return Boolean is
   begin
      if OS_Cmd_Instance.OS_Path = null then
         OS_Cmd_Instance.OS_Path :=
           GNAT.OS_Lib.Locate_Exec_On_Path (To_String (OS_Cmd_Name));

         if OS_Cmd_Instance.OS_Path /= null then
            Log.Debug
              ("found '" & To_String (OS_Cmd_Name) & "' at '" &
               OS_Cmd_Instance.OS_Path.all & "'");
         else
            Log.Debug
              ("'" & To_String (OS_Cmd_Name) & "' cannot be found in PATH");
         end if;
      end if;

      Cmd.OS_Path := OS_Cmd_Instance.OS_Path;
      return (Cmd.OS_Path /= null);
   end Check;

   ----------
   -- Path --
   ----------

   function Path (Cmd : Cmd_Type) return String is (Cmd.OS_Path.all);

   -----------------------
   -- Check_Initialized --
   -----------------------

   procedure Check_Initialized (Cmd : Cmd_Type) is
   begin
      if Cmd.OS_Path = null then
         raise Command_Error
           with "Command " & To_String (OS_Cmd_Name) & " not initialized";
      end if;
   end Check_Initialized;

   -----------
   -- Print --
   -----------

   procedure Print (Run_Output : Run_Output_Type) is
      use Ada.Text_IO;

      Output : File_Type;
      Lines  : Natural := 0;
   begin
      if Run_Output.Temp_File = null then
         Log.Debug ("Cannot print Run_Output file, null pointer");
         return;
      end if;

      Log.Debug ("Printing temp file " & Run_Output.Temp_File.all);
      Run_Output.Temp_FD.Close;

      if Dir.Size (Run_Output.Temp_File.all) > 0 then
         Output.Open (In_File, Run_Output.Temp_File.all);
         loop
            declare
               Line : constant String := Output.Get_Line;
            begin
               Log.Debug (Line);
               Lines := @ + 1;
            end;
            exit when Output.End_Of_File;
         end loop;
         Log.Debug ("(" & Lines'Image & " lines )");
         Output.Close;
      else
         Log.Debug ("( empty file )");
      end if;
   end Print;

   ---------
   -- Run --
   ---------

   function Run (Cmd : Cmd_Type; Args : String) return Run_Output_Type is
      Arg_List : GNAT.OS_Lib.Argument_List_Access;
   begin
      Check_Initialized (Cmd);

      Arg_List := GNAT.OS_Lib.Argument_String_To_List (Args);

      --  Debug all arguments
      if Log.Level = Log.Debug then
         for Arg of Arg_List.all loop
            Log.Debug ("Arg : " & Arg.all);
         end loop;
      end if;

      return Run_Output : Run_Output_Type do
         GNAT.OS_Lib.Create_Temp_Output_File
           (Run_Output.Temp_FD, Run_Output.Temp_File);

         Log.Debug ("Run " & To_String (OS_Cmd_Name) & " " & Args);
         GNAT.OS_Lib.Spawn
           (Cmd.OS_Path.all, Arg_List.all, Run_Output.Temp_FD,
            Run_Output.Return_Code);

         GNAT.OS_Lib.Free (Arg_List);
      end return;
   end Run;

   ---------
   -- Run --
   ---------

   function Run (Cmd : Cmd_Type; Args : String) return Integer is
      Arg_List : GNAT.OS_Lib.Argument_List_Access;
   begin
      Check_Initialized (Cmd);

      Arg_List := GNAT.OS_Lib.Argument_String_To_List (Args);

      --  Debug all arguments
      if Log.Level = Log.Debug then
         for Arg of Arg_List.all loop
            Log.Debug ("Arg : " & Arg.all);
         end loop;
      end if;

      return Return_Code : Integer do
         Log.Debug ("Run " & To_String (OS_Cmd_Name) & " " & Args);
         Return_Code := GNAT.OS_Lib.Spawn (Cmd.OS_Path.all, Arg_List.all);

         GNAT.OS_Lib.Free (Arg_List);
      end return;
   end Run;

   -----------
   -- Clean --
   -----------

   procedure Clean (Run_Output : in out Run_Output_Type) is
      Success : Boolean;
   begin
      if Run_Output.Temp_File = null then
         Log.Debug ("Run_Output file access is null");
      else
         if Log.Level = Log.Debug then
            Run_Output.Print;
         end if;
         Log.Debug ("Removing Run_Output file");
         GNAT.OS_Lib.Delete_File (Run_Output.Temp_File.all, Success);
      end if;
      GNAT.OS_Lib.Free (Run_Output.Temp_File);
   end Clean;

end OS_Cmd;
