-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Directories;

package body Alice.Std.OS_Cmd is

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

end Alice.Std.OS_Cmd;
