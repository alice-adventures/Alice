-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GNAT.OS_Lib;
use all type GNAT.OS_Lib.String_Access;

with Alice.IFace.OS_Cmd;
with Alice.OS_Context;
with Alice.Result;

package Alice.Std.OS_Cmd is

   type Object is new Alice.IFace.OS_Cmd.Object with private;

   type Object_Access is access all Object'Class;

   function New_Object
     (OS_Cmd_Name : String) return Alice.IFace.OS_Cmd.Object_Access
   with Pre => OS_Cmd_Name /= "";
   --  Create a new OS command object with the given name. The OS command name
   --  is the name of the executable file to run, without the path. The path
   --  is searched in the system PATH environment variable. If the command
   --  cannot be found, an error is returned.

   overriding
   function Initialize (Self : in out Object) return Alice.Result.Object'Class;

   overriding
   procedure Finalize (Self : in out Object);

   overriding
   function Is_Valid (Self : in out Object) return Boolean;

   overriding
   function Path (Self : in out Object) return String;

   overriding
   function Run
     (Self        : in out Object;
      Args        : String;
      Ctx         : Alice.OS_Context.Object;
      Exit_Status : Integer := 0) return Alice.IFace.OS_Cmd.Exit_Result'Class
   with Pre'Class => Self.Is_Valid;

   overriding
   function Run
     (Self        : in out Object;
      Args        : String;
      Ctx         : Alice.OS_Context.Object;
      Exit_Status : Integer := 0) return Alice.IFace.OS_Cmd.Output_Result'Class
   with Pre'Class => Self.Is_Valid;

   overriding
   function Timed_Run
     (Self        : in out Object;
      Args        : String;
      Ctx         : Alice.OS_Context.Object;
      Timeout     : Duration := 10.0;
      Exit_Status : Integer := 0) return Alice.IFace.OS_Cmd.Output_Result'Class
   with Pre'Class => Self.Is_Valid and then Timeout > 0.0;

   overriding
   function Cleanup
     (Self       : in out Object;
      Out_Result : in out Alice.IFace.OS_Cmd.Output_Result'Class;
      Ctx        : Alice.OS_Context.Object) return Alice.Result.Object'Class;

   procedure Debug_Output_Result
     (Out_Result : Alice.IFace.OS_Cmd.Output_Result'Class;
      Ctx        : Alice.OS_Context.Object);

private

   type Object is new Alice.IFace.OS_Cmd.Object with record
      OS_Cmd_Name : Alice.UString;
      OS_Cmd_Path : GNAT.OS_Lib.String_Access := null;
   end record;

end Alice.Std.OS_Cmd;
