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
   use all type Alice.OS_Context.Object_Access;

   -----------------------
   -- Error_Exit_Result --
   -----------------------

   function Error_Exit_Result
     (Self        : in out Object;
      Level       : Alice.Result.Error_Level;
      Message     : String;
      Exit_Status : Integer) return Alice.IFace.OS_Cmd.Exit_Result
   is (Alice.Controlled
       with
         Status      => Alice.Result.Error,
         Level       => Level,
         Message     =>
           Alice.UStr
             ("Error in command '" & Alice.Str (Self.Name) & "'': " & Message),
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
   is (Alice.Controlled
       with
         Status      => Alice.Result.Error,
         Level       => Level,
         Message     =>
           Alice.UStr
             ("Error in command '" & Alice.Str (Self.Name) & "': " & Message),
         Exit_Status => Exit_Status,
         Temp_FD     => Temp_FD,
         Temp_File   => Temp_File);

   ----------------
   -- New_OS_Cmd --
   ----------------

   function New_Object
     (Name : String; OS_Ctx : Alice.OS_Context.Object_Access)
      return Alice.IFace.OS_Cmd.Object_Access is
   begin
      return
         Instance : constant Alice.IFace.OS_Cmd.Object_Access :=
           new Alice.Std.OS_Cmd.Object'
             (Alice.Controlled
              with
                Name       => Alice.UStr (Name),
                Path       => null,
                OS_Context => OS_Ctx)
      do
         Instance.Initialize;
      end return;
   end New_Object;

   ----------------
   -- Initialize --
   ----------------

   overriding
   procedure Initialize (Self : in out Object) is
   begin
      Self.Path := GNAT.OS_Lib.Locate_Exec_On_Path (Alice.Str (Self.Name));

      if Self.Path = null then
         Self.OS_Context.Err.Exit_Application
           (Alice.Result.Error_Object'
              (Alice.Controlled
               with
                 Status  => Alice.Result.Error,
                 Level   => Alice.Result.System,
                 Message => Alice.UStr ("Initialization failed")),
            "Make sure the command """
            & Self.Name
            & """ is installed "
            & "and available in your PATH.");
      end if;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (Self : in out Object) is
   begin
      Self.Context.Log.Trace_Begin (Alice.Str (Self.Name));
      if Self.Path /= null then
         GNAT.OS_Lib.Free (Self.Path);
         Self.Path := null;
      end if;
   end Finalize;

   --------------
   -- Is_Valid --
   --------------

   overriding
   function Is_Valid (Self : in out Object) return Boolean
   is (Self.Path /= null
       and then Self.Path.all /= ""
       and then Self.OS_Context /= null);

   ----------
   -- Name --
   ----------

   overriding
   function Name (Self : in out Object) return String
   is (Alice.Str (Self.Name));

   ----------
   -- Path --
   ----------

   overriding
   function Path (Self : in out Object) return String
   is (if Self.Path /= null then Self.Path.all else "");

   -------------
   -- Context --
   -------------

   overriding
   function Context
     (Self : in out Object) return Alice.OS_Context.Object_Access
   is (Self.OS_Context);

   -------------
   -- Context --
   -------------

   overriding
   procedure Context
     (Self : in out Object; OS_Ctx : Alice.OS_Context.Object_Access) is
   begin
      Self.OS_Context := OS_Ctx;
   end Context;

   ---------
   -- Run --
   ---------

   overriding
   function Run
     (Self : in out Object; Args : String; Exit_Status : Integer := 0)
      return Alice.IFace.OS_Cmd.Exit_Result'Class
   is
      Returned_Code : Integer;
      Arg_List      : GNAT.OS_Lib.Argument_List_Access :=
        GNAT.OS_Lib.Argument_String_To_List (Args);
   begin
      Self.Context.Log.Trace_Begin
        (Alice.Str (Self.Name)
         & ", args: '"
         & Args
         & "', expect exit status:"
         & Exit_Status'Image);
      Self.Context.Log.Trace ("Run " & Alice.Str (Self.Name) & " " & Args);

      Returned_Code := GNAT.OS_Lib.Spawn (Self.Path.all, Arg_List.all);
      GNAT.OS_Lib.Free (Arg_List);

      if Returned_Code = Exit_Status then
         return
            Result : constant Alice.IFace.OS_Cmd.Exit_Result :=
              (Alice.Controlled
               with
                 Status      => Alice.Result.Success,
                 Exit_Status => Returned_Code)
         do
            Self.Context.Log.Trace_Return (Result'Image);
         end return;
      else
         return
            Result : constant Alice.IFace.OS_Cmd.Exit_Result :=
              Self.Error_Exit_Result
                (Alice.Result.System,
                 "command exit status is" & Returned_Code'Image,
                 Returned_Code)
         do
            Self.Context.Log.Trace_Return (Result'Image);
         end return;
      end if;
   end Run;

   ---------
   -- Run --
   ---------

   overriding
   function Run
     (Self : in out Object; Args : String; Exit_Status : Integer := 0)
      return Alice.IFace.OS_Cmd.Output_Result'Class
   is
      Arg_List      : GNAT.OS_Lib.Argument_List_Access :=
        GNAT.OS_Lib.Argument_String_To_List (Args);
      Returned_Code : Integer;
      Temp_FD       : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Null_FD;
      Temp_File     : GNAT.OS_Lib.String_Access := null;
   begin
      Self.Context.Log.Trace_Begin
        (Alice.Str (Self.Name)
         & ", args: '"
         & Args
         & "', expect exit status:"
         & Exit_Status'Image);
      Self.Context.Log.Trace ("Run " & Alice.Str (Self.Name) & " " & Args);

      GNAT.OS_Lib.Create_Temp_File (Temp_FD, Temp_File);
      if Temp_FD = GNAT.OS_Lib.Null_FD then
         return
            Result : constant Alice.IFace.OS_Cmd.Output_Result :=
              Self.Error_Output_Result
                (Alice.Result.System, "failed to create temporary file", 1)
         do
            Self.Context.Log.Trace_Return (Result'Image);
         end return;
      end if;

      GNAT.OS_Lib.Spawn (Self.Path.all, Arg_List.all, Temp_FD, Returned_Code);
      GNAT.OS_Lib.Free (Arg_List);

      if Returned_Code = Exit_Status then
         return
            Result : constant Alice.IFace.OS_Cmd.Output_Result :=
              (Alice.Controlled
               with
                 Status      => Alice.Result.Success,
                 Exit_Status => Returned_Code,
                 Temp_FD     => Temp_FD,
                 Temp_File   => Temp_File)
         do
            Self.Context.Log.Trace_Return (Result'Image);
         end return;
      else
         return
            Result : constant Alice.IFace.OS_Cmd.Output_Result :=
              (Alice.Controlled
               with
                 Status      => Alice.Result.Error,
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
            Self.Context.Log.Trace_Return (Result'Image);
         end return;
      end if;
   end Run;

   ---------------
   -- Timed_Run --
   ---------------

   overriding
   function Timed_Run
     (Self : in out Object; Args : String; Timeout : Duration := 1.0)
      return Alice.IFace.OS_Cmd.Output_Result'Class
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
      Self.Context.Log.Trace_Begin
        (Alice.Str (Self.Name)
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
            Self.Context.Log.Trace_Return (Result'Image);
         end return;
      end if;

      Spawned_PID :=
        GNAT.OS_Lib.Non_Blocking_Spawn
          (Self.Path.all, Arg_List.all, Temp_FD, True);

      Finished : Boolean := False;
      Remaining_Time : Duration := Timeout;
      Δ_Time : constant Duration := Timeout / 10.0;

      loop
         exit when Remaining_Time <= 0.0 or else Finished;
         delay Δ_Time;
         Remaining_Time := @ - Δ_Time;

         GNAT.OS_Lib.Non_Blocking_Wait_Process (PID, Success);

         if PID = Spawned_PID then
            Self.Context.Log.Trace ("Process finished with PID: " & PID'Image);
            Finished := True;
         end if;
      end loop;

      if not Finished then
         Is_Timeout := True;
         GNAT.OS_Lib.Kill_Process_Tree (Spawned_PID);
      end if;

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
            Self.Context.Log.Trace_Return (Result'Image);
         end return;
      else
         return
            Result : constant Alice.IFace.OS_Cmd.Output_Result :=
              (Alice.Controlled
               with
                 Status      => Alice.Result.Success,
                 Exit_Status => 0,
                 Temp_FD     => Temp_FD,
                 Temp_File   => Temp_File)
         do
            Self.Context.Log.Trace_Return (Result'Image);
         end return;
      end if;
   end Timed_Run;

   -------------
   -- Cleanup --
   -------------

   overriding
   function Cleanup
     (Self   : in out Object;
      Result : in out Alice.IFace.OS_Cmd.Output_Result'Class)
      return Alice.Result.Object'Class is
   begin
      Self.Context.Log.Trace_Begin (Result'Image);

      case Result.Status is
         when Alice.Result.Success | Alice.Result.Error =>
            if Result.Temp_File = null
              and then Result.Temp_FD = GNAT.OS_Lib.Null_FD
            then
               return
                  Result : constant Alice.Result.Success_Object :=
                    (Alice.Controlled
                     with Status => Alice.Result.Success)
               do
                  Self.Context.Log.Trace ("No temporary file to clean up");
                  Self.Context.Log.Trace_Return (Result'Image);
               end return;
            else
               Success : Boolean;

               Self.Context.Log.Trace
                 ("Deleting temporary file " & Result.Temp_File.all);
               GNAT.OS_Lib.Delete_File (Result.Temp_File.all, Success);
               GNAT.OS_Lib.Free (Result.Temp_File);
               Result.Exit_Status := -1;
               Result.Temp_FD := GNAT.OS_Lib.Null_FD;
               Result.Temp_File := null;

               if Success then
                  return
                     Result : constant Alice.Result.Success_Object :=
                       (Alice.Controlled
                        with Status => Alice.Result.Success)
                  do
                     Self.Context.Log.Trace_Return (Result'Image);
                  end return;
               else
                  return
                     Result : constant Alice.Result.Error_Object :=
                       (Alice.Controlled
                        with
                          Status  => Alice.Result.Error,
                          Level   => Alice.Result.System,
                          Message =>
                            Alice.UStr
                              ("Failed to delete temporary file "
                               & Alice.Str (Self.Name)))
                  do
                     Self.Context.Log.Trace_Return (Result'Image);
                  end return;
               end if;
            end if;
      end case;
   end Cleanup;

   -------------------------
   -- Debug_Output_Result --
   -------------------------

   overriding
   procedure Debug_Output_Result
     (Self   : in out Object;
      Result : in out Alice.IFace.OS_Cmd.Output_Result'Class)
   is
      use Ada.Directories;
      use Ada.Text_IO;

      Temp_File : File_Type;
      Lines     : Natural := 0;
   begin
      if Result.Temp_File = null then
         Self.Context.Log.Debug ("No output file to print");
      else
         Self.Context.Log.Debug
           ("Output file: "
            & Result.Temp_File.all
            & " (FD: "
            & Result.Temp_FD'Image
            & ")");

         if Size (Result.Temp_File.all) = File_Size (0) then
            Self.Context.Log.Debug ("Output file is empty");
         else
            Open (Temp_File, In_File, Result.Temp_File.all);
            loop
               Line : constant String := Get_Line (Temp_File);

               Self.Context.Log.Debug (Line);
               Lines := Lines + 1;
               exit when End_Of_File (Temp_File);
            end loop;
            Self.Context.Log.Debug ("[EOF] Total of" & Lines'Image & " lines");
            Close (Temp_File);
         end if;
      end if;
   end Debug_Output_Result;

end Alice.Std.OS_Cmd;
