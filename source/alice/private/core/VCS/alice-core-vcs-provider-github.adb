-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.Result;

package body Alice.Core.VCS.Provider.GitHub is

   ----------------------------
   -- Get_Profile_From_Token --
   ----------------------------

   overriding
   function Get_Profile_From_Token
     (Self : in out Object; Token : String)
      return Alice.Core.VCS.Profile.Result.Object'Class
   is (Alice.Core.VCS.Profile.Result.Create (Alice.Result.Success, null));
   --  #FIXME - Provide a proper implementation to retrieve the profile from
   --  the token. This function should interact with the GitHub API to fetch
   --  the user profile associated with the provided token. The implementation
   --  should handle the API request, parse the response, and return a valid
   --  profile object. If the token is invalid or the request fails, it should
   --  return an error result.

   --------------------------------------
   -- Get_Profile_From_VCS_Config_File --
   --------------------------------------

   overriding
   function Get_Profile_From_VCS_Config_File
     (Self : in out Object; Token : String)
      return Alice.Core.VCS.Profile.Result.Object'Class
   is (Alice.Core.VCS.Profile.Result.Create (Alice.Result.Success, null));
   --  #FIXME - Provide a proper implementation to retrieve the profile from
   --  the VCS configuration file. This function should read the VCS
   --  configuration file (e.g., '~/.gitconfig') and extract the profile
   --  information associated with the provided token. The implementation
   --  should handle file reading, parsing the configuration, and returning a
   --  valid profile object. If the token is invalid or the profile does not
   --  exist, it should return an error result.

   ----------------------------------
   -- Get_Profile_From_Config_File --
   ----------------------------------

   overriding
   function Get_Profile_From_Alice_Config_File
     (Self : in out Object; File : String)
      return Alice.Core.VCS.Profile.Result.Object'Class
   is (Alice.Core.VCS.Profile.Result.Create (Alice.Result.Success, null));
   --  #FIXME - Provide a proper implementation to retrieve the profile from
   --  the Alice configuration file. This function should read the specified
   --  configuration file and extract the profile information. The
   --  implementation should handle file reading, parsing the configuration,
   --  and returning a valid profile object. If the file does not exist or the
   --  profile is not found, it should return an error result.

end Alice.Core.VCS.Provider.GitHub;
