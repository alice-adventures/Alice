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

   Key_Avatar_URL     : constant String := "avatar_url";
   Key_Email          : constant String := "email";
   Key_Login          : constant String := "login";
   Key_Name           : constant String := "name";
   Key_Type           : constant String := "type";
   Key_User_View_Type : constant String := "user_view_type";

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
           Alice.VCS.Profile.Result.Create_Object
             (Status  => Alice.Result.Success,
              Profile =>
                Alice.VCS.Profile.Create_Profile
                  (User_Name   => Alice.UStr (Value (Key_Name)),
                   User_Email  => Alice.UStr (Value (Key_Email)),
                   User_Login  => Alice.UStr (Value (Key_Login)),
                   User_Avatar => Alice.UStr (Value (Key_Avatar_URL)),
                   User_Token  => Alice.UStr (Token)));
      else
         return
           Alice.VCS.Profile.Result.Create_Object
             (Status        => Alice.Result.Error,
              Profile       => null,
              Error_Level   => Alice.Result.External,
              Error_Message =>
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
           Alice.VCS.Profile.Result.Create_Object
             (Status        => Alice.Result.Error,
              Profile       => null,
              Error_Level   => Alice.Result.External,
              Error_Message =>
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

   ---------------------------------------------
   -- Get_Member_Profile_From_VCS_Config_File --
   ---------------------------------------------

   overriding
   function Get_Member_Profile_From_VCS_Config_File
     (Self : in out Object; Token : String)
      return Alice.VCS.Profile.Result.Object'Class
   is (Alice.VCS.Profile.Result.Create_Object (Alice.Result.Success, null));
   --  #REVIEW - Really needed?
   --
   --  #TODO - Provide a proper implementation to retrieve the profile from
   --  the VCS configuration file. This function should read the VCS
   --  configuration file (e.g., '~/.gitconfig') and extract the profile
   --  information associated with the provided token. The implementation
   --  should handle file reading, parsing the configuration, and returning a
   --  valid profile object. If the token is invalid or the profile does not
   --  exist, it should return an error result.

   -----------------------------------------------
   -- Get_Member_Profile_From_Alice_Config_File --
   -----------------------------------------------

   overriding
   function Get_Member_Profile_From_Alice_Config_File
     (Self : in out Object; File : String)
      return Alice.VCS.Profile.Result.Object'Class
   is (Alice.VCS.Profile.Result.Create_Object (Alice.Result.Success, null));
   --  #TODO - Provide a proper implementation to retrieve the profile from
   --  the Alice configuration file. This function should read the specified
   --  configuration file and extract the profile information. The
   --  implementation should handle file reading, parsing the configuration,
   --  and returning a valid profile object. If the file does not exist or the
   --  profile is not found, it should return an error result.

   ---------------------------
   -- Get_Member_Repository --
   ---------------------------

   overriding
   function Get_Member_Repository
     (Self    : in out Object;
      Profile : Alice.VCS.Profile.Object'Class;
      Name    : String) return Alice.Result.Object'Class
   is (Alice.Result.Success_Object'
         (Alice.Controlled with Status => Alice.Result.Success));
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
   is (Alice.Result.Success_Object'
         (Alice.Controlled with Status => Alice.Result.Success));
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
   is (Alice.Result.Success_Object'
         (Alice.Controlled with Status => Alice.Result.Success));
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
           Alice.Result.Success_Object'
             (Alice.Controlled with Status => Alice.Result.Success);
      else
         return
           Alice.Result.Create_Error
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
   is (Alice.Result.Success_Object'
         (Alice.Controlled with Status => Alice.Result.Success));
   --  #TODO - Provide a proper implementation

end Alice.VCS.Service.GitHub;
