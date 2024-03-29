-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Integer_Text_IO;

with AAA.Strings;
with Simple_Logging;

with OS_Cmd_Curl;

package body GitHub.API is

   package Log renames Simple_Logging;

   Flags      : constant String := " -s -L";
   HTTP_Code  : constant String := " -w %{http_code}\\n";
   Output     : constant String := " -o " & JSON_File;
   Accept_Hdr : constant String := " -H Accept:\ application/vnd.github+json";
   Auth_Hdr   : constant String := " -H Authorization:\ Bearer\ ";
   Base_URL   : constant String := " https://api.github.com/";

   ------------
   -- Quoted --
   ------------

   function Quoted (Text : String) return String is ("""" & Text & """");

   -------------
   -- Escaped --
   -------------

   function Escaped (Text : String) return String is
     (AAA.Strings.Replace (AAA.Strings.Crunch (Text), " ", "\ "));

   ---------------
   -- Key_Value --
   ---------------

   function Key_Value (Key, Value : String) return String is
     (Quoted (Key) & ":" & Quoted (Escaped (Value)));

   --------------
   -- JSON_Obj --
   --------------

   function JSON_Obj (Text : String) return String is
     ("{" & AAA.Strings.Replace (Text, """", "\""") & "}");

   ---------------
   -- Curl_Args --
   ---------------

   function Curl_Args (Profile : Profile_Type) return String is
     (Flags & HTTP_Code & Output & Accept_Hdr & Auth_Hdr & Profile.Token &
      Base_URL);

   ------------------
   -- Send_Request --
   ------------------

   function Send_Request
     (Request : String; Contents : String := "") return Natural
   is
      Curl_Cmd      : OS_Cmd_Curl.Cmd_Type;
      Run_Output    : OS_Cmd_Curl.Run_Output_Type;
      Response_File : Ada.Text_IO.File_Type;
   begin
      Curl_Cmd.Init;

      Log.Debug ("Request = " & Request);
      if Contents'Length = 0 then
         Run_Output := Curl_Cmd.Run (Request);
      else
         Log.Debug ("Contents = " & Contents);
         Run_Output := Curl_Cmd.Run (Request & " -d " & Contents);
      end if;

      return HTTP_Code : Natural do
         Response_File.Open
           (Mode => Ada.Text_IO.In_File, Name => Run_Output.Temp_File.all);
         Ada.Integer_Text_IO.Get (Response_File, HTTP_Code);
         Log.Debug ("Response =" & HTTP_Code'Image);

         Response_File.Close;
         Run_Output.Clean;
      end return;
   end Send_Request;

   ----------------------------------------------------
   -- Create_A_Repository_For_The_Authenticated_User --
   ----------------------------------------------------

   procedure Create_A_Repository_For_The_Authenticated_User
     (Profile : Profile_Type; Repo : String; Description : String)
   is
      Request  : constant String := Curl_Args (Profile) & "user/repos";
      Contents : constant String :=
        JSON_Obj
          (Key_Value ("name", Repo) & "," &
           Key_Value ("description", Description) & "," &
           Key_Value ("auto_init", "true"));
   begin
      if Send_Request (Request, Contents) /= 201 then
         raise GitHub_Api_Error
           with "Cannot create a repository for the authenticated user";
      end if;
   end Create_A_Repository_For_The_Authenticated_User;

   ------------------------------------------
   -- Create_A_Repository_Using_A_Template --
   ------------------------------------------

   function Create_A_Repository_Using_A_Template
     (Profile     : Profile_Type; Template : String; Repo : String;
      Description : String) return Boolean
   is
      Request  : constant String :=
        Curl_Args (Profile) & "repos/alice-adventures/" & Template &
        "/generate";
      Contents : constant String :=
        JSON_Obj
          (Key_Value ("owner", Profile.Login) & "," &
           Key_Value ("name", Repo) & "," &
           Key_Value ("description", Description));
   begin
      return (Send_Request (Request, Contents) = 201);
   end Create_A_Repository_Using_A_Template;

   ----------------------
   -- Get_A_Repository --
   ----------------------

   function Get_A_Repository
     (Profile : Profile_Type; Owner : String; Repo : String) return Boolean
   is
      Request : constant String :=
        Curl_Args (Profile) & "repos/" & Owner & "/" & Repo;
   begin
      return (Send_Request (Request) = 200);
   end Get_A_Repository;

   ----------------
   -- Get_A_User --
   ----------------

   function Get_A_User (Profile : Profile_Type; Name : String) return Boolean
   is
      Request : constant String := Curl_Args (Profile) & "users/" & Name;
   begin
      return (Send_Request (Request) = 200);
   end Get_A_User;

   --------------------------------
   -- Get_The_Authenticated_User --
   --------------------------------

   procedure Get_The_Authenticated_User (Profile : Profile_Type) is
      Request : constant String := Curl_Args (Profile) & "user";
   begin
      if Send_Request (Request) /= 200 then
         raise GitHub_Api_Error with "Cannot get the authenticated user";
      end if;
   end Get_The_Authenticated_User;

   ----------------------------------
   -- List_Repositories_For_A_User --
   ----------------------------------

   function List_Repositories_For_A_User
     (Profile : Profile_Type; User : String) return Boolean
   is
      Request : constant String :=
        Curl_Args (Profile) & "users/" & User & "/repos" &
        "?type=owner&sort=full_name&direction=asc&per_page=100";
   begin
      return (Send_Request (Request) = 200);
   end List_Repositories_For_A_User;

   --------------------------------------------------
   -- List_Repositories_For_The_Authenticated_User --
   --------------------------------------------------

   function List_Repositories_For_The_Authenticated_User
     (Profile : Profile_Type) return Boolean
   is
      Request : constant String :=
        Curl_Args (Profile) & "user/repos" &
        "?visibility=public&affiliation=owner&sort=full_name&direction=asc" &
        "&since=2023-07-12T00:00:00Z";
   begin
      return (Send_Request (Request) = 200);
   end List_Repositories_For_The_Authenticated_User;

end GitHub.API;
