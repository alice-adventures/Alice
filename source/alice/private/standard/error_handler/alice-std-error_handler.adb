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

   overriding
   function Handle_Error
     (Self : in out Object; Result : Alice.Result.Error_Object'Class)
      return Boolean is
   begin
      Simple_Logging.Error (Alice.Str (Result.Message));
      case Result.Level is
         when Alice.Result.Bug =>
            Self.Exit_Application (Result);

         when others =>
            return True;
      end case;
   end Handle_Error;

   overriding
   procedure Exit_Application
     (Self : in out Object; Result : Alice.Result.Object'Class)
   is
      use Alice.IFace.Error_Handler;
      Exit_Code : Exit_Code_Value;
   begin
      case Result.Status is
         when Alice.Result.Success =>
            Exit_Code := Success;

         when Alice.Result.Error =>
            case Result.Level is
               when Alice.Result.Bug =>
                  Exit_Code := Bug;

               when Alice.Result.Domain =>
                  Exit_Code := Error;

               when Alice.Result.System =>
                  Exit_Code := System;

               when Alice.Result.External =>
                  Exit_Code := External;
            end case;
      end case;

      GNAT.OS_Lib.OS_Exit (Exit_Code_Value'Enum_Rep (Exit_Code));
   end Exit_Application;

end Alice.Std.Error_Handler;
