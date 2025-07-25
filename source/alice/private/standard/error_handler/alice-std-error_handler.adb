-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GNAT.OS_Lib;

with Simple_Logging;

package body Alice.Std.Error_Handler is

   ---------
   -- Log --
   ---------

   overriding
   procedure Log
     (Self     : in out Object;
      Message  : String;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity;
      Location : String := GNAT.Source_Info.Source_Location) is
   begin
      Simple_Logging.Error (Message);
   end Log;

   ------------------
   -- Handle_Error --
   ------------------

   overriding
   function Handle_Error
     (Self : in out Object; Result : Alice.Result.Error_Object'Class)
      return Boolean is
   begin
      case Result.Status is
         when Alice.Result.Success =>
            return False;

         when Alice.Result.Error =>
            case Result.Level is
               when Alice.Result.Bug =>
                  Self.Exit_Application
                    (Result,
                     Alice.UStr
                       ("A bug has been detected in the code. "
                        & "Please report it to the developers."));

               when others =>
                  Simple_Logging.Error (Alice.Str (Result.Message));
                  return True;
            end case;
      end case;
   end Handle_Error;

   ----------------------
   -- Exit_Application --
   ----------------------

   overriding
   procedure Exit_Application
     (Self    : in out Object;
      Level   : Alice.Result.Error_Level;
      Explain : Alice.UString := Alice.Null_UString)
   is
      use Alice.IFace.Error_Handler;
      Exit_Code : Exit_Code_Value;
   begin
      case Level is
         when Alice.Result.Bug =>
            Exit_Code := Bug;

         when Alice.Result.Domain =>
            Exit_Code := Error;

         when Alice.Result.Timeout | Alice.Result.System =>
            Exit_Code := System;

         when Alice.Result.External =>
            Exit_Code := External;
      end case;

      if Explain /= Alice.Null_UString then
         Simple_Logging.Error (Alice.Str (Explain));
      end if;

      GNAT.OS_Lib.OS_Exit (Exit_Code_Value'Enum_Rep (Exit_Code));
   end Exit_Application;

   ----------------------
   -- Exit_Application --
   ----------------------

   overriding
   procedure Exit_Application
     (Self    : in out Object;
      Result  : Alice.Result.Object'Class;
      Explain : Alice.UString := Alice.Null_UString)
   is
      use Alice.IFace.Error_Handler;
   begin
      case Result.Status is
         when Alice.Result.Success =>
            GNAT.OS_Lib.OS_Exit (Exit_Code_Value'Enum_Rep (Success));

         when Alice.Result.Error =>
            Simple_Logging.Error (Alice.Str (Result.Message));
            Self.Exit_Application (Result.Level, Explain);
      end case;
   end Exit_Application;

end Alice.Std.Error_Handler;
