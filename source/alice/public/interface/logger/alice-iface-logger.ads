-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package defines the Logger interface used in the Alice application.
--
--  It allows logging messages at different levels, such as Info, Detail, and
--  Debug. The logging can be optimized for CLI or GUI applications, and the
--  output is redirected to the standard output or standard error, depending
--  on the optimization.
--
--  Typical usage of the package starts by selecting the optimization CLI/GUI,
--  and then selecting the appropriate verbose/trace/debug level.
--
--  The logging levels are defined as follows:
--
--     * Info is used for additional or verbose information that is not an
--       error or warning
--
--     * Trace is used for observability, such as performance or traceability
--       information, usually not shown to the user (no-op in release builds)
--
--     * Debug is used for debugging information, usually not shown to the
--       user, but useful for developers (no-op in release builds)
--
--  In development and validation builds the default level is Debug. In
--  release builds the default level is Warning.

with GNAT.Source_Info; use GNAT.Source_Info;

package Alice.IFace.Logger is

   type Object is interface and Alice.IFace.Object;
   --  This interface defines the contract for loggers in the Alice
   --  application. It allows different implementations to log messages in a
   --  consistent manner.

   type Object_Access is not null access all Object'Class;
   --  Object_Access is an access type for the Object interface. It allows
   --  for dynamic dispatch and polymorphism, enabling different logger
   --  implementations to be used interchangeably.

   procedure Optimize_For_CLI
     (Self : in out Object; With_Color_Enabled : Boolean := True)
   is abstract;
   --  Optimize the logging for CLI applications. This sets the level to
   --  Warning, redirects all messages to the standard output and enables the
   --  progress tracker for CLI.

   procedure Optimize_For_GUI (Self : in out Object)
   is abstract; -- #TODO - Add an abstract Status Spinner parameter
   --  Optimize the logging for GUI applications. This sets the level to Info
   --  (verbose), uses the instance of the busy Status spinner and redirects
   --  all messages to the standard error.

   procedure Set_Default_Level (Self : in out Object) is abstract;
   --  Set the default logging level. In development and validation builds the
   --  default level is Debug. In release builds the default level is Warning.

   procedure Set_Verbose_Level (Self : in out Object) is abstract;
   --  Set the logging level to Verbose (aka Info).

   procedure Set_Trace_Level
     (Self : in out Object; With_Location_Enabled : Boolean := True)
   is abstract;
   --  Set the logging level to Trace. Must be a no-op in release builds.

   procedure Set_Debug_Level
     (Self : in out Object; With_Location_Enabled : Boolean := True)
   is abstract;
   --  Set the logging level to Debug. Must be a no-op in release builds.

   procedure Info
     (Self     : in out Object;
      Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location)
   is abstract;
   --  Log an info message: additional or verbose information that is not an
   --  error or warning. The message is sent to the standard error. This is
   --  the verbose level in release builds.

   procedure Warning
     (Self     : in out Object;
      Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location)
   is abstract;
   --  Log a warning message: indicates a potential problem or an unexpected
   --  situation that is not an error.

   procedure Trace_Begin
     (Self     : in out Object;
      Msg      : String := "";
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location)
   is abstract;
   --  Log a trace begin message: debugging information, usually not shown to
   --  the user, but useful for developers. The message is sent to the
   --  standard error. Should be a no-op in release builds.

   procedure Trace
     (Self     : in out Object;
      Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location)
   is abstract;
   --  Log a trace message: observability, such as performance or traceability
   --  information, usually not shown to the user. Should be a no-op in release
   --  builds.

   procedure Trace_Return
     (Self     : in out Object;
      Msg      : String := "";
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location)
   is abstract;
   --  Log a trace return message: indicates the return from a function,
   --  usually with the return value as a parameter. Should be a no-op in
   --  release builds.

   procedure Trace_End
     (Self     : in out Object;
      Msg      : String := "";
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location)
   is abstract;
   --  Log a trace end message: debugging information, usually not shown to
   --  the user, but useful for developers. Should be a no-op in release
   --  builds.

   procedure Debug
     (Self     : in out Object;
      Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location)
   is abstract;
   --  Log a debug message: debugging information, usually not shown to the
   --  user, but useful for developers. Should be a no-op in release builds.

   procedure Save_State (Self : in out Object) is abstract;
   --  Save the current state of the logger. This is useful for restoring the
   --  logger state after a change in the logging level or optimization.

   procedure Restore_State (Self : in out Object) is abstract;
   --  Restore the state of the logger to the last saved state. This is useful
   --  for reverting changes made to the logger, such as changing the logging
   --  level or optimization.

end Alice.IFace.Logger;
