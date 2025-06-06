-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

--  This package provides a simple logging interface for the ALICE library. It
--  allows logging messages at different levels, such as Info, Detail, and
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

package Alice.Log is

   procedure Optimize_For_CLI (With_Color_Enabled : Boolean := True);
   --  Optimize the logging for CLI applications. This sets the level to
   --  Warning, redirects all messages to the standard output and enables the
   --  busy status spinner for CLI.

   procedure Optimize_For_GUI; -- #TODO - Add an abstract Spinner parameter
   --  Optimize the logging for GUI applications. This sets the level to Info
   --  (verbose), uses the instance of the busy status spinner and redirects
   --  all messages to the standard error.

   procedure Set_Verbose_Level (Verbose : Boolean);
   --  When True, set the logging level to Info, otherwise set it to Warning.

   procedure Set_Trace_Level (With_Location_Enabled : Boolean := True);
   --  Set the logging level to Trace. This is a no-op in release builds.

   procedure Set_Debug_Level (With_Location_Enabled : Boolean := True);
   --  Set the logging level to Debug. This is a no-op in release builds.

   procedure Info
     (Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location);
   --  Log an info message: additional or verbose information that is not an
   --  error or warning. The message is sent to the standard error. This is
   --  the verbose level in release builds.

   procedure Trace_Begin
     (Msg      : String := "";
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location);
   --  Log a trace begin message: debugging information, usually not shown to
   --  the user, but useful for developers. The message is sent to the
   --  standard error. This is a no-op in release builds.

   procedure Trace
     (Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location);
   --  Log a trace message: observability, such as performance or traceability
   --  information, usually not shown to the user. This is a no-op in release
   --  builds.

   procedure Trace_Return
     (Msg      : String := "";
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location);
      --  Log a trace return message: indicates the return from a function,
      --  usually with the return value as a parameter. This is a no-op in
      --  release builds.

   procedure Trace_End
     (Msg      : String := "";
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location);
   --  Log a trace end message: debugging information, usually not shown to
   --  the user, but useful for developers. This is a no-op in release builds.

   procedure Debug
     (Msg      : String;
      Entity   : String := Enclosing_Entity;
      Location : String := Source_Location);
   --  Log a debug message: debugging information, usually not shown to the
   --  user, but useful for developers. This is a no-op in release builds.

end Alice.Log;
