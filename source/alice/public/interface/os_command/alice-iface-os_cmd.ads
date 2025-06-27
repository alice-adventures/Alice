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
         when Alice.Result.Success | Alice.Result.Error =>
            Exit_Status : Integer;
      end case;
   end record;
   --  A record to hold the exit code of a command when Success. It is used
   --  when the command is run without saving the output to a temporary file.
   --  The Return_Code is usually equal to zero when the command executed
   --  successfully, but in some cases other values can be considered also a
   --  success.

   Null_Exit_Result : constant Exit_Result :=
     (Status => Alice.Result.Success, Exit_Status => 0);
   --  A null result for commands. It is used as default value for variables
   --  of type Exit_Result.

   type Output_Result (Status : Alice.Result.Status_Type) is
     new Alice.Result.Object (Status)
   with record
      case Status is
         when Alice.Result.Success | Alice.Result.Error =>
            Exit_Status : Integer;

            Temp_FD : GNAT.OS_Lib.File_Descriptor := GNAT.OS_Lib.Null_FD;
            --  The file descriptor of the temporary file where the output is
            --  saved.

            Temp_File : GNAT.OS_Lib.String_Access := null;
            --  The temporary filename where the output is saved.
      end case;
   end record;
   --  A record to hold the exit code and the output of a command. This is
   --  used when the command output is saved to a temporary file.

   Null_Output_Result : constant Output_Result :=
     (Status      => Alice.Result.Success,
      Exit_Status => -1,
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

   function Is_Valid (Self : in out Object) return Boolean is abstract;
   --  Check if the OS command has been initialized and is valid. This is used
   --  to check if the command can be run before running it. If the command is
   --  not valid, it cannot be run and an error is returned. It is useful to
   --  avoid running commands that are not available on the system, such as
   --  when the command is not installed or the command is not found in PATH.

   function Path (Self : in out Object) return String is abstract;
   --  Return the PATH where the OS command is found.

   function Run
     (Self        : in out Object;
      Args        : String;
      Ctx         : Alice.OS_Context.Object;
      Exit_Status : Integer := 0) return Exit_Result'Class
   is abstract
   with Pre'Class => Self.Is_Valid;
   --  Run the command with the given arguments and return the command exit
   --  status. The standard output and error streams are not saved. This is
   --  useful for commands that do not produce output or when the output is
   --  not needed. The exit status is usually zero when the command executed
   --  successfully, but in some cases other values can be considered also a
   --  success, such as when the command is run with a specific exit code that
   --  indicates a successful operation, such as `git pull` which returns a
   --  non-zero exit code when there are no changes to pull, but the command
   --  is still considered successful. So, if the exit status of the command
   --  is equal to the Exit_Status parameter, the command is considered
   --  successful.

   function Run
     (Self        : in out Object;
      Args        : String;
      Ctx         : Alice.OS_Context.Object;
      Exit_Status : Integer := 0) return Output_Result'Class
   is abstract
   with Pre'Class => Self.Is_Valid;
   --  Run the command with the given arguments. Return the exit code and a
   --  file with the output. The standard output and error streams are saved
   --  to a temporary file. When the exit status of the command is equal to
   --  the Exit_Status parameter, the command is considered successful.

   function Timed_Run
     (Self        : in out Object;
      Args        : String;
      Ctx         : Alice.OS_Context.Object;
      Timeout     : Duration := 1.0) return Output_Result'Class
   is abstract
   with Pre'Class => Self.Is_Valid and then Timeout >= 1.0;
   --  Run the command with the given arguments and a timeout. If the command
   --  does not finish within the timeout, it is killed and an error is
   --  returned. The standard output and error streams are saved to a
   --  temporary file. The return value is the exit code and the temporary
   --  file with the output. This is useful for commands that may take a long
   --  time to finish, such as downloading files or running long computations.
   --  The timeout is used to prevent the command from running indefinitely
   --  and blocking the application.

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
