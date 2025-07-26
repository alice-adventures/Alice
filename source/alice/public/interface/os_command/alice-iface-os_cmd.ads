-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package provides a generic interface to run operating system
--  commands.

with Ada.Strings.Text_Buffers;
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
   end record
   with Put_Image => Put_Image_Output_Result;
   --  A record to hold the exit code and the output of a command. This is
   --  used when the command output is saved to a temporary file.

   function Is_Valid (Self : in out Object) return Boolean is abstract;
   --  Check if the OS command has been initialized and is valid. This is used
   --  to check if the command can be run before running it. If the command is
   --  not valid, it cannot be run and an error is returned. It is useful to
   --  avoid running commands that are not available on the system, such as
   --  when the command is not installed or the command is not found in PATH.

   function Name (Self : in out Object) return String is abstract;
   --  Return the name of the OS command. This is the name of the executable
   --  file to run, without the path. The path is searched in the system PATH
   --  environment variable.

   function Path (Self : in out Object) return String is abstract
   with Pre'Class => Self.Is_Valid;
   --  Return the PATH where the OS command is found.

   function Context
     (Self : in out Object) return Alice.OS_Context.Object_Access
   is abstract
   with Pre'Class => Self.Is_Valid;
   --  Return the OS context where the command is run. This is used to access
   --  the error handler and logger for the command. It is useful to log
   --  messages and handle errors that occur during the command execution.

   procedure Context
     (Self : in out Object; Ctx : Alice.OS_Context.Object_Access)
   is abstract
   with Pre'Class => Self.Is_Valid;
   --  Set the OS context where the command is run. This is used to set the
   --  error handler and logger for the command. It is useful to log messages
   --  and handle errors that occur during the command execution. The context
   --  is usually set when the command is created, but it can be changed later
   --  if needed.

   function Run
     (Self : in out Object; Args : String; Exit_Status : Integer := 0)
      return Exit_Result'Class
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
     (Self : in out Object; Args : String; Exit_Status : Integer := 0)
      return Output_Result'Class
   is abstract
   with Pre'Class => Self.Is_Valid;
   --  Run the command with the given arguments. Return the exit code and a
   --  file with the output. The standard output and error streams are saved
   --  to a temporary file. When the exit status of the command is equal to
   --  the Exit_Status parameter, the command is considered successful.

   function Timed_Run
     (Self : in out Object; Args : String; Timeout : Duration)
      return Output_Result'Class
   is abstract
   with Pre'Class => Self.Is_Valid and then Timeout > 0.0;
   --  Run the command with the given arguments and a timeout. If the command
   --  does not finish within the timeout, it is killed and an error is
   --  returned. The standard output and error streams are saved to a
   --  temporary file. The return value is the exit code and the temporary
   --  file with the output. This is useful for commands that may take a long
   --  time to finish, such as downloading files or running long computations.
   --  The timeout is used to prevent the command from running indefinitely
   --  and blocking the application.

   function Cleanup
     (Self : in out Object; Result : in out Output_Result'Class)
      return Alice.Result.Object'Class
   is abstract;
   --  Clean the output of a command. This is used to delete temporary files
   --  and free allocated memory by the command output.

   procedure Debug_Output_Result
     (Self : in out Object; Result : in out Output_Result'Class)
   is abstract;
   --  Debug the output of a command. This is used to print the output of the
   --  command to the log. It is useful for debugging purposes to see the
   --  output of the command and check if it is correct. The output is printed
   --  to the log with the debug level.

   procedure Put_Image_Output_Result
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Self   : Output_Result);

end Alice.IFace.OS_Cmd;
