-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package provides the standard implementation for the Alice
--  application, which is enough for the CLI. It is implemented on top of
--  Simple_Logging.

with GNAT.Source_Info; use GNAT.Source_Info;

with Alice.IFace.Logger;

package Alice.Std.Log is

   type Object is new Alice.IFace.Logger.Object with null record;

   overriding
   procedure Optimize_For_CLI
     (Self : in out Object; With_Color_Enabled : Boolean := True);
   --  Optimize the logging for CLI applications. This sets the level to
   --  Warning, redirects all messages to the standard output and enables the
   --  busy status spinner for CLI.

   overriding
   procedure Optimize_For_GUI
     (Self : in out Object); -- #TODO - Add an abstract Spinner parameter
   --  Optimize the logging for GUI applications. This sets the level to Info
   --  (verbose), uses the instance of the busy status spinner and redirects
   --  all messages to the standard error.

   overriding
   procedure Set_Verbose_Level (Self : in out Object; Verbose : Boolean);
   --  When True, set the logging level to Info, otherwise set it to Warning.

   overriding
   procedure Set_Trace_Level
     (Self : in out Object; With_Location_Enabled : Boolean := True);
   --  Set the logging level to Trace. This is a no-op in release builds.

   overriding
   procedure Set_Debug_Level
     (Self : in out Object; With_Location_Enabled : Boolean := True);
   --  Set the logging level to Debug. This is a no-op in release builds.

   overriding
   procedure Info
     (Self     : in out Object;
      Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location);
   --  Log an info message: additional or verbose information that is not an
   --  error or warning. The message is sent to the standard error. This is
   --  the verbose level in release builds.

   overriding
   procedure Warning
     (Self     : in out Object;
      Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location);
   --  Log a warning message: a condition that indicates a potential problem
   --  or an unexpected situation that is not an error.

   overriding
   procedure Trace_Begin
     (Self     : in out Object;
      Msg      : String := "";
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location);
   --  Log a trace begin message: debugging information, usually not shown to
   --  the user, but useful for developers. The message is sent to the
   --  standard error. This is a no-op in release builds.

   overriding
   procedure Trace
     (Self     : in out Object;
      Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location);
   --  Log a trace message: observability, such as performance or traceability
   --  information, usually not shown to the user. This is a no-op in release
   --  builds.

   overriding
   procedure Trace_Return
     (Self     : in out Object;
      Msg      : String := "";
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location);
   --  Log a trace return message: indicates the return from a function,
   --  usually with the return value as a parameter. This is a no-op in
   --  release builds.

   overriding
   procedure Trace_End
     (Self     : in out Object;
      Msg      : String := "";
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location);
   --  Log a trace end message: debugging information, usually not shown to
   --  the user, but useful for developers. This is a no-op in release builds.

   overriding
   procedure Debug
     (Self     : in out Object;
      Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location);
   --  Log a debug message: debugging information, usually not shown to the
   --  user, but useful for developers. This is a no-op in release builds.

end Alice.Std.Log;
