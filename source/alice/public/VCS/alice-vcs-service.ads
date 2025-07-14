-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.Context;
with Alice.Std;

package Alice.VCS.Service is

   function Send_Request
     (Request  : String;
      Contents : String := "";
      Timeout  : Duration := 5.0;
      Context  : Alice.Context.Object_Access := Alice.Std.Get_Context)
      return Natural;
   --  Send a request to a VCS service using the 'curl' command. This function
   --  constructs the request, sends it to the VCS service, and returns the
   --  HTTP response code. The Contents parameter allows sending data with the
   --  request, usually a JSON payload. If Contents is empty, the request is
   --  sent without a body. The Context parameter allows specifying the
   --  context in which the command is executed, defaulting to the current OS
   --  context. The function returns the HTTP response code as a Natural
   --  value.

end Alice.VCS.Service;
