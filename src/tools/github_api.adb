-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Integer_Text_IO;

with OS_Cmd;
with OS_Cmd.Curl; use OS_Cmd.Curl;

with Simple_Logging;

package body GitHub_API is

   Flags      : constant String := " -s -L";
   HTTP_Code  : constant String := " -w %{http_code}\\n";
   Output     : constant String := " -o " & JSON_File;
   Accept_Hdr : constant String := " -H Accept:\ application/vnd.github+json";
   Auth_Hdr   : constant String := " -H Authorization:\ Bearer\ ";
   Base_URL   : constant String := " https://api.github.com/";

   package Log renames Simple_Logging;

   ---------------
   -- Curl_Args --
   ---------------

   function Curl_Args (User_Config : User_Config_Type) return String is
     (Flags & HTTP_Code & Output & Accept_Hdr & Auth_Hdr &
      To_String (User_Config.Token) & Base_URL);

   ------------------
   -- Send_Request --
   ------------------

   function Send_Request (Request : String) return Natural is
      Curl_Cmd      : Curl_Cmd_Type;
      Run_Output    : OS_Cmd.Run_Output_Type;
      HTTP_Code     : Natural;
      Response_File : Ada.Text_IO.File_Type;
   begin
      if not Curl_Cmd.Init then
         return 404;
      end if;

      Log.Debug ("Request = " & Request);
      Run_Output := Curl_Cmd.Run (Request);

      Response_File.Open
        (Mode => Ada.Text_IO.In_File, Name => Run_Output.Temp_File.all);
      Ada.Integer_Text_IO.Get (Response_File, HTTP_Code);
      Log.Debug ("Response =" & HTTP_Code'Image);
      Response_File.Close;

      Curl_Cmd.Clean (Run_Output);

      return HTTP_Code;
   end Send_Request;

   ----------------------------------------------------
   -- Create_A_Repository_For_The_Authenticated_User --
   ----------------------------------------------------

   function Create_A_Repository_For_The_Authenticated_User
     (User_Config : User_Config_Type; Repo : String; Description : String)
      return Boolean is
     (True);
   --  *TODO - Implementation

   ------------------------------------------
   -- Create_A_Repository_Using_A_Template --
   ------------------------------------------

   function Create_A_Repository_Using_A_Template
     (User_Config : User_Config_Type; Template : String; Repo : String;
      Description : String) return Boolean is
     (True);
   --  *TODO - Implementation

   ----------------------
   -- Get_A_Repository --
   ----------------------

   function Get_A_Repository
     (User_Config : User_Config_Type; Repo : String) return Boolean
   is
      Request : constant String :=
        Curl_Args (User_Config) & "repos/" & To_String (User_Config.Login) &
        "/" & Repo;
   begin
      return (Send_Request (Request) = 200);
   end Get_A_Repository;

   ----------------
   -- Get_A_User --
   ----------------

   function Get_A_User
     (User_Config : User_Config_Type; Name : String) return Boolean is
     (True);
   --  *TODO - Implementation

   --------------------------------
   -- Get_The_Authenticated_User --
   --------------------------------

   function Get_The_Authenticated_User
     (User_Config : User_Config_Type) return Boolean
   is
      Request : constant String := Curl_Args (User_Config) & "user";
   begin
      return (Send_Request (Request) = 200);
   end Get_The_Authenticated_User;

   ------------------------------
   -- List_Public_Repositories --
   ------------------------------

   function List_Public_Repositories
     (User_Config : User_Config_Type) return Boolean is
     (True);
   --  *TODO - Implementation

   ----------------------------------
   -- List_Repositories_For_A_User --
   ----------------------------------

   function List_Repositories_For_A_User
     (User_Config : User_Config_Type; User : String) return Boolean is
     (True);
   --  *TODO - Implementation

end GitHub_API;
