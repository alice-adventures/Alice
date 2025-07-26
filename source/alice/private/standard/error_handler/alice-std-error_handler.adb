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

   use all type Alice.Hint.Id;

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

   overriding
   procedure Exit_Application
     (Self        : in out Object;
      Error_Level : Alice.Result.Error_Level;
      Hint_Id     : Alice.Hint.Id) is
   begin
      Self.Exit_Application (Error_Level, Alice.Hint.Get_Message (Hint_Id));
   end Exit_Application;

   ----------------------
   -- Exit_Application --
   ----------------------

   overriding
   procedure Exit_Application
     (Self        : in out Object;
      Error_Level : Alice.Result.Error_Level;
      Explain     : Alice.UString := Alice.Null_UString)
   is
      use Alice.IFace.Error_Handler;
      Exit_Code : Exit_Code_Value;
   begin
      case Error_Level is
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
            if Result.Hint /= Alice.Hint.None then
               Simple_Logging.Error (Alice.Str (Result.Hint.Get_Message));
            end if;
            Self.Exit_Application (Result.Level, Explain);
      end case;
   end Exit_Application;

   -------------------------
   -- Error_Handler_Image --
   -------------------------

   procedure Put_Image_Error_Handler
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Self   : Object) is
   begin
      Output.Put ("([" & Self'Address'Image & " ])");
   end Put_Image_Error_Handler;

end Alice.Std.Error_Handler;
