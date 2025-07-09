-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package body Alice.VCS.Service.GitHub is

   -----------------------------------
   -- Get_Member_Profile_From_Token --
   -----------------------------------

   overriding
   function Get_Member_Profile_From_Token
     (Self : in out Object; Token : String)
      return Alice.VCS.Profile.Result.Object'Class
   is (Alice.VCS.Profile.Result.Create (Alice.Result.Success, null));
   --  #FIXME - Provide a proper implementation to retrieve the profile from
   --  the token. This function should interact with the GitHub API to fetch
   --  the user profile associated with the provided token. The implementation
   --  should handle the API request, parse the response, and return a valid
   --  profile object. If the token is invalid or the request fails, it should
   --  return an error result.

   ---------------------------------------------
   -- Get_Member_Profile_From_VCS_Config_File --
   ---------------------------------------------

   overriding
   function Get_Member_Profile_From_VCS_Config_File
     (Self : in out Object; Token : String)
      return Alice.VCS.Profile.Result.Object'Class
   is (Alice.VCS.Profile.Result.Create (Alice.Result.Success, null));
   --  #FIXME - Provide a proper implementation to retrieve the profile from
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
   is (Alice.VCS.Profile.Result.Create (Alice.Result.Success, null));
   --  #FIXME - Provide a proper implementation to retrieve the profile from
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
   --  #FIXME - Provide a proper implementation

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
   --  #FIXME - Provide a proper implementation

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
   --  #FIXME - Provide a proper implementation

   --------------
   -- Get_User --
   --------------

   overriding
   function Get_User
     (Self : in out Object; Name : String) return Alice.Result.Object'Class
   is (Alice.Result.Success_Object'
         (Alice.Controlled with Status => Alice.Result.Success));
   --  #FIXME - Provide a proper implementation

   -------------------------
   -- Get_User_Repository --
   -------------------------

   overriding
   function Get_User_Repository
     (Self : in out Object; Token : String) return Alice.Result.Object'Class
   is (Alice.Result.Success_Object'
         (Alice.Controlled with Status => Alice.Result.Success));
   --  #FIXME - Provide a proper implementation

end Alice.VCS.Service.GitHub;
