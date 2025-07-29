-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with JSON.Types;
with JSON.Parsers;

package body Alice.VCS.Service.GitHub is

   Output_JSON_File : constant String := ".github.json";
   --  The output file where the response from the GitHub API will be stored.

   Accept_Header : constant String := "Accept:\ application/vnd.github+json";
   --  The header to specify the desired response format from the GitHub API.

   Auth_Header : constant String := "Authorization:\ Bearer\ ";
   --  The header to include the authorization token for accessing the GitHub
   --  API.

   Version_Header : constant String := "X-GitHub-Api-Version:\ 2022-11-28";
   --  The header to specify the version of the GitHub API being used.

   Base_URL : constant String := "https://api.github.com/";
   --  The base URL for the GitHub API. This is used to construct API
   --  endpoints for various operations such as fetching user profiles,
   --  repositories, etc.

   ---------------
   -- Curl_Args --
   ---------------

   function Curl_Args (Token : String; Endpoint : String) return String
   is (" -s -L -w %{http_code}\\n "
       & " -o "
       & Output_JSON_File
       & " -H "
       & Accept_Header
       & " -H "
       & Auth_Header
       & Token
       & " -H "
       & Version_Header
       & " "
       & Base_URL
       & Endpoint);

   ------------------------------------
   -- Get_Profile_From_Response_File --
   ------------------------------------

   function Get_Profile_From_Output_JSON_File
     (Token : String) return Alice.VCS.Profile.Result.Object'Class
   is
      package JSON_Types is new JSON.Types (Integer, Float);
      package JSON_Parsers is new JSON.Parsers (JSON_Types);

      use JSON_Types;

      Profile_Parser : JSON_Parsers.Parser :=
        JSON_Parsers.Create_From_File (File_Name => Output_JSON_File);
      JSON_Object    : constant JSON_Value := Profile_Parser.Parse;

      -------------
      -- Has_Key --
      -------------

      function Has_Key (Key : String) return Boolean
      is (JSON_Object.Contains (Key));

      ---------------
      -- Is_String --
      ---------------

      function Is_String (Key : String) return Boolean
      is (JSON_Object.Get (Key).Kind = String_Kind)
      with Pre => Has_Key (Key);

      -----------
      -- Value --
      -----------

      function Value (Key : String) return String
      is (if Is_String (Key)
          then JSON_Types.Value (JSON_Object.Get (Key))
          else "");

   begin
      if Has_Key (Key_Login)
        and then Is_String (Key_Login)
        and then Has_Key (Key_Type)
        and then Is_String (Key_Type)
        and then Value (Key_Type) = "User"
        and then Has_Key (Key_User_View_Type)
        and then Is_String (Key_User_View_Type)
        and then Value (Key_User_View_Type) = "public"
      then
         return
           Alice.VCS.Profile.Result.Success
             (Alice.VCS.Profile.Create_Profile
                (Service    => Alice.VCS.Service.Name.GitHub,
                 Token      => Alice.UStr (Token),
                 Login      => Alice.UStr (Value (Key_Login)),
                 Avatar_URL => Alice.UStr (Value (Key_Avatar_URL)),
                 Name       => Alice.UStr (Value (Key_Name)),
                 Email      => Alice.UStr (Value (Key_Email))));
      else
         return
           Alice.VCS.Profile.Result.Error
             (Alice.Result.External,
              Alice.UStr
                ("Error fetching member profile: Invalid login"
                 & ", type or public view type."));
      end if;
   end Get_Profile_From_Output_JSON_File;

   -----------------------------------
   -- Get_Member_Profile_From_Token --
   -----------------------------------

   overriding
   function Get_Member_Profile_From_Token
     (Self : in out Object; Token : String)
      return Alice.VCS.Profile.Result.Object'Class is
   begin
      HTTP_Code : constant Natural := Send_Request (Curl_Args (Token, "user"));

      if HTTP_Code = 200 then
         return Get_Profile_From_Output_JSON_File (Token);

      else
         return
           Alice.VCS.Profile.Result.Error
             (Alice.Result.External,
              Alice.UStr
                ("Error fetching member profile: HTTP status code"
                 & Natural'Image (HTTP_Code)
                 & (case HTTP_Code is
                      when 304 => " - Not modified.",
                      when 401 => " - Unauthorized.",
                      when 403 => " - Forbidden.",
                      when others => " - Unexpected error occurred.")));
      end if;
   end Get_Member_Profile_From_Token;

   ---------------------------
   -- Get_Member_Repository --
   ---------------------------

   overriding
   function Get_Member_Repository
     (Self    : in out Object;
      Profile : Alice.VCS.Profile.Object'Class;
      Name    : String) return Alice.Result.Object'Class
   is (Alice.Result.Success);
   --  #TODO - Provide a proper implementation

   ------------------------------
   -- Create_Member_Repository --
   ------------------------------

   overriding
   function Create_Member_Repository
     (Self        : in out Object;
      Profile     : Alice.VCS.Profile.Object'Class;
      Name        : String;
      Description : String) return Alice.Result.Object'Class
   is (Alice.Result.Success);
   --  #TODO - Provide a proper implementation

   --------------------------------------------
   -- Create_Member_Repository_From_Template --
   --------------------------------------------

   overriding
   function Create_Member_Repository_From_Template
     (Self        : in out Object;
      Profile     : Alice.VCS.Profile.Object'Class;
      Template    : String;
      Name        : String;
      Description : String) return Alice.Result.Object'Class
   is (Alice.Result.Success);
   --  #TODO - Provide a proper implementation

   --------------
   -- Get_User --
   --------------

   overriding
   function Get_User
     (Self : in out Object; Name : String) return Alice.Result.Object'Class
   is
      HTTP_Code : Natural;
   begin
      HTTP_Code :=
        Send_Request
          (Request  =>
             " -s -L"
             & " -w %{http_code}\\n"
             & " -o .github.json"
             & " -H Accept:\ application/vnd.github+json"
             & " -H Authorization:\ Bearer\ " -- #TODO - Use the token here
             & Base_URL
             & "users/"
             & Name,
           Contents => "");

      if HTTP_Code = 200 then
         return
           Alice.Result.Success;
      else
         return
           Alice.Result.Error
             (Alice.Result.Timeout,
              Alice.UStr
                ("Error fetching user profile: HTTP code "
                 & Natural'Image (HTTP_Code)));
      end if;
   end Get_User;
   --  #TODO - Provide a proper implementation - should return a profile
   --  result

   -------------------------
   -- Get_User_Repository --
   -------------------------

   overriding
   function Get_User_Repository
     (Self : in out Object; Token : String) return Alice.Result.Object'Class
   is (Alice.Result.Success);
   --  #TODO - Provide a proper implementation

end Alice.VCS.Service.GitHub;
