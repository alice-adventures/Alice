-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package provides a generic interface to run operating system
--  commands.

with GNAT.OS_Lib;

with Alice.OS_Context;
with Alice.Result;

package Alice.IFace.OS_Cmd is

   type Object is interface and Alice.IFace.Object;

   type Object_Access is not null access all Object'Class;

   type Exit_Result (Status : Alice.Result.Status_Type) is
     new Alice.Result.Object (Status)
   with record
      case Status is
         when Alice.Result.Success =>
            Return_Code : Integer;
            --  The command exited returning a code.

         when Alice.Result.Error =>
            null;
      end case;
   end record;
   --  A record to hold the exit code of a command.

   Null_Exit_Result : constant Exit_Result :=
     (Status => Alice.Result.Success, Return_Code => 0);
   --  A null result for commands. It is used as default value for variables
   --  of type Exit_Result.

   type Output_Result (Status : Alice.Result.Status_Type) is
     new Alice.Result.Object (Status)
   with record
      case Status is
         when Alice.Result.Success =>
            Return_Code : Integer;
            --  The command exited returning a code.
            Temp_FD     : GNAT.OS_Lib.File_Descriptor;
            --  The file descriptor of the temporary file where the output is
            --  saved.
            Temp_File   : GNAT.OS_Lib.String_Access;
            --  The temporary filename where the output is saved.

         when Alice.Result.Error =>
            null;
      end case;
   end record;
   --  A record to hold the exit code and the output of a command. This is
   --  used when the command output is saved to a temporary file.

   Null_Output_Result : constant Output_Result :=
     (Status      => Alice.Result.Success,
      Return_Code => 0,
      Temp_FD     => GNAT.OS_Lib.Null_FD,
      Temp_File   => null);
   --  A null result for commands. It is used as default value for variables
   --  of type Output_Result.

   overriding
   function Initialize
     (Self : in out Object) return Alice.Result.Object'Class is abstract;
   --  Initialize an OS command, or raise an exception if the command cannot
   --  be found in PATH.

   overriding
   procedure Finalize (Self : in out Object) is abstract;
   --  Finalize an OS command. Delete temporary files and free allocated
   --  memory by the command.

   --  Ctx.OS_Cmd.Alr.Init (Ctx.Error_Handler, Ctx.Log);
   --  Ctx.OS_Cmd.Git.Run (Ctx.Core, "log --oneline --graph --decorate");

   --  function Check
   --    (Self : in out Object; Ctx : Alice.OS_Context.Object)
   --     return Boolean
   --  is abstract;
   --  Initialize an OS command by trying to find the executable file in PATH.
   --  Return True if the OS command can be used.

   function Path (Self : in out Object) return String is abstract;
   --  Return the PATH where the OS command is found.

   function Run
     (Self : in out Object; Args : String; Ctx : Alice.OS_Context.Object)
      return Exit_Result'Class
   is abstract;
   --  Run the command with the given arguments and return the command exit
   --  code. The standard output and error streams are not saved.

   function Run
     (Self : in out Object; Args : String; Ctx : Alice.OS_Context.Object)
      return Output_Result'Class
   is abstract;
   --  Run the command with the given arguments. Return the exit code and a
   --  file with the output. The standard output and error streams are saved
   --  to a temporary file.

   function Cleanup
     (Self       : in out Object;
      Out_Result : in out Output_Result'Class;
      Ctx        : Alice.OS_Context.Object) return Alice.Result.Object'Class
   is abstract;
   --  Clean the output of a command. This is used to delete temporary files
   --  and free allocated memory by the command output.

private

   --  type Cmd_Type is new Ada.Finalization.Limited_Controlled with record
   --     OS_Path : aliased GNAT.OS_Lib.String_Access := null;
   --  end record;

   --  type Object is tagged record with
   --     Cmd_Path : aliased GNAT.OS_Lib.String_Access := null;
   --  end record;

end Alice.IFace.OS_Cmd;
