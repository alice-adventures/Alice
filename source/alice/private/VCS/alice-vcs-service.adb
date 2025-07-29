-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Integer_Text_IO;
with Ada.Text_IO;

with Alice.IFace;
with Alice.IFace.OS_Cmd;
with Alice.Result;

package body Alice.VCS.Service is

   ------------------
   -- Send_Request --
   ------------------

   function Send_Request
     (Request  : String;
      Contents : String := "";
      Timeout  : Duration := 5.0;
      Context  : Alice.Context.Object_Access := Alice.Std.Get_Context)
      return Natural
   is
      Curl_Cmd   : constant Alice.IFace.OS_Cmd.Object_Access :=
        Context.OS_Cmd.Curl;
      Args       : constant String :=
        (if Contents'Length = 0 then Request else Request & " -d " & Contents);
      Run_Output : Alice.IFace.OS_Cmd.Result_Output'Class :=
        Curl_Cmd.Timed_Run (Args, Timeout);
   begin
      Context.Log.Debug (Run_Output'Image);
      return HTTP_Code : Natural do
         case Run_Output.Status is
            when Alice.Result.Success =>
               Response_File : Ada.Text_IO.File_Type;
               Response_File.Open
                 (Mode => Ada.Text_IO.In_File,
                  Name => Run_Output.Temp_File.all);
               Ada.Integer_Text_IO.Get (Response_File, HTTP_Code);
               Response_File.Close;

            when Alice.Result.Error =>
               HTTP_Code := 408; -- Request Timeout
         end case;
         Ignore : Alice.Result.Object'Class := Curl_Cmd.Cleanup (Run_Output);
      end return;
   end Send_Request;

end Alice.VCS.Service;
