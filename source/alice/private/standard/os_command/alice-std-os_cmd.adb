-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Text_IO;

package body Alice.Std.OS_Cmd is

   use all type Ada.Directories.File_Size;
   use all type GNAT.OS_Lib.File_Descriptor;

   -----------------------
   -- Error_Exit_Result --
   -----------------------

   function Error_Exit_Result
     (Self        : in out Object;
      Level       : Alice.Result.Error_Level;
      Message     : String;
      Exit_Status : Integer) return Alice.IFace.OS_Cmd.Exit_Result
   is (Status      => Alice.Result.Error,
       Level       => Level,
       Message     =>
         Alice.UStr
           ("Error in command '"
            & Alice.Str (Self.OS_Cmd_Name)
            & "'': "
            & Message),
       Exit_Status => Exit_Status);

   -------------------------
   -- Error_Output_Result --
   -------------------------

   function Error_Output_Result
     (Self        : in out Object;
      Level       : Alice.Result.Error_Level;
      Message     : String;
      Exit_Status : Integer;
      Temp_FD     : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Null_FD;
      Temp_File   : GNAT.OS_Lib.String_Access := null)
      return Alice.IFace.OS_Cmd.Output_Result
   is (Status      => Alice.Result.Error,
       Level       => Level,
       Message     =>
         Alice.UStr
           ("Error in command '"
            & Alice.Str (Self.OS_Cmd_Name)
            & "': "
            & Message),
       Exit_Status => Exit_Status,
       Temp_FD     => Temp_FD,
       Temp_File   => Temp_File);

   ----------------
   -- New_OS_Cmd --
   ----------------

   function New_Object
     (OS_Cmd_Name : String) return Alice.IFace.OS_Cmd.Object_Access
   is (new Object'
         (OS_Cmd_Name => Alice.UStr (OS_Cmd_Name), OS_Cmd_Path => null));

   ----------------
   -- Initialize --
   ----------------

   overriding
   function Initialize (Self : in out Object) return Alice.Result.Object'Class
   is
   begin
      Self.OS_Cmd_Path :=
        GNAT.OS_Lib.Locate_Exec_On_Path (Str (Self.OS_Cmd_Name));

      if Self.OS_Cmd_Path = null then
         return
            Result : constant Alice.Result.Error_Object :=
              (Status  => Alice.Result.Error,
               Level   => Alice.Result.System,
               Message =>
                 Alice.UStr
                   ("Command '"
                    & Alice.Str (Self.OS_Cmd_Name)
                    & "' not found in PATH"));
      else
         return Result : Alice.Result.Success_Object;
      end if;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Self : in out Object) is
   begin
      if Self.OS_Cmd_Path /= null then
         GNAT.OS_Lib.Free (Self.OS_Cmd_Path);
         Self.OS_Cmd_Path := null;
      end if;
   end Finalize;

   --------------
   -- Is_Valid --
   --------------

   overriding
   function Is_Valid (Self : in out Object) return Boolean
   is (Self.Path /= "");

   ----------
   -- Path --
   ----------

   overriding
   function Path (Self : in out Object) return String
   is (if Self.OS_Cmd_Path /= null then Self.OS_Cmd_Path.all else "");

   ---------
   -- Run --
   ---------

   overriding
   function Run
     (Self        : in out Object;
      Args        : String;
      Ctx         : Alice.OS_Context.Object;
      Exit_Status : Integer := 0) return Alice.IFace.OS_Cmd.Exit_Result'Class
   is
      Returned_Code : Integer;
      Arg_List      : GNAT.OS_Lib.Argument_List_Access :=
        GNAT.OS_Lib.Argument_String_To_List (Args);
   begin
      Ctx.Log.Trace_Begin
        (Str (Self.OS_Cmd_Name)
         & ", args: '"
         & Args
         & "', expect exit status:"
         & Exit_Status'Image);
      Ctx.Log.Trace ("Run " & Alice.Str (Self.OS_Cmd_Name) & " " & Args);

      Returned_Code := GNAT.OS_Lib.Spawn (Self.OS_Cmd_Path.all, Arg_List.all);
      GNAT.OS_Lib.Free (Arg_List);

      if Returned_Code = Exit_Status then
         return
            Result : constant Alice.IFace.OS_Cmd.Exit_Result :=
              (Status => Alice.Result.Success, Exit_Status => Returned_Code)
         do
            Ctx.Log.Trace_Return (Result'Image);
         end return;
      else
         return
            Result : constant Alice.IFace.OS_Cmd.Exit_Result :=
              Self.Error_Exit_Result
                (Alice.Result.System,
                 "command exit status is" & Returned_Code'Image,
                 Returned_Code)
         do
            Ctx.Log.Trace_Return (Result'Image);
         end return;
      end if;
   end Run;

   ---------
   -- Run --
   ---------

   overriding
   function Run
     (Self        : in out Object;
      Args        : String;
      Ctx         : Alice.OS_Context.Object;
      Exit_Status : Integer := 0) return Alice.IFace.OS_Cmd.Output_Result'Class
   is
      Arg_List      : GNAT.OS_Lib.Argument_List_Access :=
        GNAT.OS_Lib.Argument_String_To_List (Args);
      Returned_Code : Integer;
      Temp_FD       : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Null_FD;
      Temp_File     : GNAT.OS_Lib.String_Access := null;
   begin
      Ctx.Log.Trace_Begin
        (Str (Self.OS_Cmd_Name)
         & ", args: '"
         & Args
         & "', expect exit status:"
         & Exit_Status'Image);
      Ctx.Log.Trace ("Run " & Alice.Str (Self.OS_Cmd_Name) & " " & Args);

      GNAT.OS_Lib.Create_Temp_File (Temp_FD, Temp_File);
      if Temp_FD = GNAT.OS_Lib.Null_FD then
         return
            Result : constant Alice.IFace.OS_Cmd.Output_Result :=
              Self.Error_Output_Result
                (Alice.Result.System, "failed to create temporary file", 1)
         do
            Ctx.Log.Trace_Return (Result'Image);
         end return;
      end if;

      GNAT.OS_Lib.Spawn
        (Self.OS_Cmd_Path.all, Arg_List.all, Temp_FD, Returned_Code);
      GNAT.OS_Lib.Free (Arg_List);

      if Returned_Code = Exit_Status then
         return
            Result : constant Alice.IFace.OS_Cmd.Output_Result :=
              (Status      => Alice.Result.Success,
               Exit_Status => Returned_Code,
               Temp_FD     => Temp_FD,
               Temp_File   => Temp_File)
         do
            Ctx.Log.Trace_Return (Result'Image);
         end return;
      else
         return
            Result : constant Alice.IFace.OS_Cmd.Output_Result :=
              (Status      => Alice.Result.Error,
               Level       => Alice.Result.System,
               Message     =>
                 Alice.UStr
                   ("command exit status is "
                    & Returned_Code'Image
                    & ", expected "
                    & Exit_Status'Image),
               Exit_Status => Returned_Code,
               Temp_FD     => Temp_FD,
               Temp_File   => Temp_File)
         do
            Ctx.Log.Trace_Return (Result'Image);
         end return;
      end if;
   end Run;

   ---------------
   -- Timed_Run --
   ---------------

   overriding
   function Timed_Run
     (Self    : in out Object;
      Args    : String;
      Ctx     : Alice.OS_Context.Object;
      Timeout : Duration := 1.0) return Alice.IFace.OS_Cmd.Output_Result'Class
   is
      use all type GNAT.OS_Lib.Process_Id;

      Is_Timeout  : Boolean := False;
      Arg_List    : GNAT.OS_Lib.Argument_List_Access :=
        GNAT.OS_Lib.Argument_String_To_List (Args);
      Temp_FD     : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Null_FD;
      Temp_File   : GNAT.OS_Lib.String_Access := null;
      PID         : GNAT.OS_Lib.Process_Id;
      Spawned_PID : GNAT.OS_Lib.Process_Id;
      Success     : Boolean;

   begin
      Ctx.Log.Trace_Begin
        (Str (Self.OS_Cmd_Name)
         & ", args: '"
         & Args
         & "', max timeout: "
         & Timeout'Image
         & " seconds");

      GNAT.OS_Lib.Create_Temp_File (Temp_FD, Temp_File);
      if Temp_FD = GNAT.OS_Lib.Null_FD then
         return
            Result : constant Alice.IFace.OS_Cmd.Output_Result :=
              Self.Error_Output_Result
                (Alice.Result.System, "failed to create temporary file", 1)
         do
            Ctx.Log.Trace_Return (Result'Image);
         end return;
      end if;

      Spawned_PID :=
        GNAT.OS_Lib.Non_Blocking_Spawn
          (Self.OS_Cmd_Path.all, Arg_List.all, Temp_FD, True);

      declare
         Finished       : Boolean := False;
         Remaining_Time : Duration := Timeout;
         Δ_Time         : constant Duration := Timeout / 10.0;
      begin
         loop
            exit when Remaining_Time <= 0.0 or else Finished;
            delay Δ_Time;
            Remaining_Time := @ - Δ_Time;

            GNAT.OS_Lib.Non_Blocking_Wait_Process (PID, Success);

            if PID = Spawned_PID then
               Ctx.Log.Trace ("Process finished with PID: " & PID'Image);
               Finished := True;
            end if;
         end loop;
         if not Finished then
            Is_Timeout := True;
            GNAT.OS_Lib.Kill_Process_Tree (Spawned_PID);
         end if;
      end;
      GNAT.OS_Lib.Free (Arg_List);

      if Is_Timeout then
         return
            Result : constant Alice.IFace.OS_Cmd.Output_Result :=
              Self.Error_Output_Result
                (Alice.Result.Timeout,
                 "command timed out after " & Timeout'Image & " seconds",
                 1,
                 Temp_FD,
                 Temp_File)
         do
            Ctx.Log.Trace_Return (Result'Image);
         end return;
      else
         return
            Result : constant Alice.IFace.OS_Cmd.Output_Result :=
              (Status      => Alice.Result.Success,
               Exit_Status => 0,
               Temp_FD     => Temp_FD,
               Temp_File   => Temp_File)
         do
            Ctx.Log.Trace_Return (Result'Image);
         end return;
      end if;
   end Timed_Run;

   -------------
   -- Cleanup --
   -------------

   overriding
   function Cleanup
     (Self       : in out Object;
      Out_Result : in out Alice.IFace.OS_Cmd.Output_Result'Class;
      Ctx        : Alice.OS_Context.Object) return Alice.Result.Object'Class is
   begin
      Ctx.Log.Trace_Begin (Out_Result'Image);

      case Out_Result.Status is
         when Alice.Result.Success | Alice.Result.Error =>
            if Out_Result.Temp_File = null
              and then Out_Result.Temp_FD = GNAT.OS_Lib.Null_FD
            then
               return
                  Result : constant Alice.Result.Success_Object :=
                    (Status => Alice.Result.Success)
               do
                  Ctx.Log.Trace ("No temporary file to clean up");
                  Ctx.Log.Trace_Return (Result'Image);
               end return;
            else
               declare
                  Success : Boolean;
               begin
                  Ctx.Log.Trace
                    ("Deleting temporary file " & Out_Result.Temp_File.all);
                  GNAT.OS_Lib.Delete_File (Out_Result.Temp_File.all, Success);
                  GNAT.OS_Lib.Free (Out_Result.Temp_File);
                  Out_Result.Exit_Status := -1;
                  Out_Result.Temp_FD := GNAT.OS_Lib.Null_FD;
                  Out_Result.Temp_File := null;
                  --    Alice.IFace.OS_Cmd.Output_Result'Class
                  --      (Alice.IFace.OS_Cmd.Null_Output_Result);
                  if Success then
                     return
                        Result : constant Alice.Result.Success_Object :=
                          (Status => Alice.Result.Success)
                     do
                        Ctx.Log.Trace_Return (Result'Image);
                     end return;
                  else
                     return
                        Result : constant Alice.Result.Error_Object :=
                          (Status  => Alice.Result.Error,
                           Level   => Alice.Result.System,
                           Message =>
                             Alice.UStr
                               ("Failed to delete temporary file "
                                & Alice.Str (Self.OS_Cmd_Name)))
                     do
                        Ctx.Log.Trace_Return (Result'Image);
                     end return;
                  end if;
               end;
            end if;

            --  when Alice.Result.Error =>
            --     return
            --        Result : constant Alice.Result.Error_Object :=
            --          (Status  => Alice.Result.Error,
            --           Level   => Alice.Result.Bug,
            --           Message =>
            --             Alice.UStr
            --               ("Unexpected status in Cleanup: "
            --                & Out_Result.Status'Image))
            --     do
            --        Ctx.Log.Trace_Return (Result'Image);
            --     end return;
      end case;
   end Cleanup;

   -------------------------
   -- Debug_Output_Result --
   -------------------------

   procedure Debug_Output_Result
     (Out_Result : Alice.IFace.OS_Cmd.Output_Result'Class;
      Ctx        : Alice.OS_Context.Object)
   is
      use Ada.Directories;
      use Ada.Text_IO;

      Temp_File : File_Type;
      Lines     : Natural := 0;
   begin
      Ctx.Log.Trace_Begin (Out_Result'Image);

      if Out_Result.Temp_File = null then
         Ctx.Log.Debug ("No output file to print");
      else
         Ctx.Log.Debug
           ("Output file: "
            & Out_Result.Temp_File.all
            & " (FD: "
            & Out_Result.Temp_FD'Image
            & ")");

         if Size (Out_Result.Temp_File.all) = File_Size (0) then
            Ctx.Log.Debug ("Output file is empty");
         else
            Open (Temp_File, In_File, Out_Result.Temp_File.all);
            loop
               declare
                  Line : constant String := Get_Line (Temp_File);
               begin
                  Ctx.Log.Debug (Line);
                  Lines := Lines + 1;
                  exit when End_Of_File (Temp_File);
               end;
            end loop;
            Ctx.Log.Debug ("[EOF] Total of" & Lines'Image & " lines");
            Close (Temp_File);
         end if;
      end if;

      Ctx.Log.Trace_End;
   end Debug_Output_Result;

end Alice.Std.OS_Cmd;
