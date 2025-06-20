-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;

package body Alice.Std.OS_Cmd is

   use all type GNAT.OS_Lib.File_Descriptor;

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
     (Self : in out Object; Args : String; Ctx : Alice.OS_Context.Object)
      return Alice.IFace.OS_Cmd.Exit_Result'Class is
   begin
      Ctx.Log.Trace_Begin;
      if Self.OS_Cmd_Path = null then
         return
            Result : constant Alice.IFace.OS_Cmd.Exit_Result :=
              (Status  => Alice.Result.Error,
               Level   => Alice.Result.Bug,
               Message =>
                 Alice.UStr
                   ("Command '"
                    & Alice.Str (Self.OS_Cmd_Name)
                    & "' not initialized"))
         do
            Ctx.Log.Trace_Return (Result'Image);
         end return;
      else
         declare
            Return_Code : Integer;
            Arg_List    : GNAT.OS_Lib.Argument_List_Access :=
              GNAT.OS_Lib.Argument_String_To_List (Args);
         begin
            Ctx.Log.Trace ("Run " & Alice.Str (Self.OS_Cmd_Name) & " " & Args);
            Return_Code :=
              GNAT.OS_Lib.Spawn (Self.OS_Cmd_Path.all, Arg_List.all);
            GNAT.OS_Lib.Free (Arg_List);
            return
               Result : constant Alice.IFace.OS_Cmd.Exit_Result :=
                 (Status => Alice.Result.Success, Return_Code => Return_Code)
            do
               Ctx.Log.Trace_Return (Result'Image);
            end return;
         end;
      end if;
   end Run;

   ---------
   -- Run --
   ---------

   overriding
   function Run
     (Self : in out Object; Args : String; Ctx : Alice.OS_Context.Object)
      return Alice.IFace.OS_Cmd.Output_Result'Class is
   begin
      Ctx.Log.Trace_Begin;
      if Self.OS_Cmd_Path = null then
         return
            Result : constant Alice.IFace.OS_Cmd.Output_Result :=
              (Status  => Alice.Result.Error,
               Level   => Alice.Result.Bug,
               Message =>
                 Alice.UStr
                   ("Command '"
                    & Alice.Str (Self.OS_Cmd_Name)
                    & "' not initialized"))
         do
            Ctx.Log.Trace_Return (Result'Image);
         end return;
      else
         declare
            Arg_List : GNAT.OS_Lib.Argument_List_Access :=
              GNAT.OS_Lib.Argument_String_To_List (Args);
         begin
            Ctx.Log.Trace ("Run " & Alice.Str (Self.OS_Cmd_Name) & " " & Args);
            return
               Result :
                 Alice.IFace.OS_Cmd.Output_Result
                   (Status => Alice.Result.Success)
            do
               GNAT.OS_Lib.Create_Temp_File (Result.Temp_FD, Result.Temp_File);
               GNAT.OS_Lib.Spawn
                 (Self.OS_Cmd_Path.all,
                  Arg_List.all,
                  Result.Temp_FD,
                  Result.Return_Code);
               GNAT.OS_Lib.Free (Arg_List);
               Ctx.Log.Trace_Return (Result'Image);
            end return;
         end;
      end if;
   end Run;

   -------------
   -- Cleanup --
   -------------

   overriding
   function Cleanup
     (Self       : in out Object;
      Out_Result : in out Alice.IFace.OS_Cmd.Output_Result'Class;
      Ctx        : Alice.OS_Context.Object) return Alice.Result.Object'Class is
   begin
      Ctx.Log.Trace_Begin ("Cleanup of: " & Out_Result'Image);

      case Out_Result.Status is
         when Alice.Result.Success =>

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
                  Out_Result :=
                    Alice.IFace.OS_Cmd.Output_Result'Class
                      (Alice.IFace.OS_Cmd.Null_Output_Result);
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

         when Alice.Result.Error =>
            return
               Result : constant Alice.Result.Error_Object :=
                 (Status  => Alice.Result.Error,
                  Level   => Alice.Result.Bug,
                  Message =>
                    Alice.UStr
                      ("Unexpected status in Cleanup: "
                       & Out_Result.Status'Image))
            do
               Ctx.Log.Trace_Return (Result'Image);
            end return;
      end case;
   end Cleanup;

end Alice.Std.OS_Cmd;
