-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------
--
--  This package provides the Alice Participant profile management. The
--  profile is a set of information that is used to identify the user and to
--  store the required information to use the Alice tools. There are two main
--  profiles: the local profile and the remote profile. The local profile is
--  the profile used by the local participant, the one which is actually
--  running the Alice tools. The remote profile is the profile of a different
--  participant that is used to share collaboration data, such as pieces of
--  code, progress or project updates, within the Alice environment.
--
--  The profile is currently implemented by the GitHub.Profile.Profile_Type
--  because there is a need to store the GitHub token and the GitHub user
--  name. If in the future exists the possibility to use Alice with other git
--  providers, then the profile will be changed to a more generic/abstract
--  type.

with GitHub.Profile; use GitHub.Profile;

package Alice_Participant is

   Participant_Error : exception;

   function Has_Local_Profile return Boolean;
   --  Check if the local profile file exists on the filesystem. This function
   --  does not validate the file's format or contents, only its presence.

   function Get_Local_Profile
     (Refresh_From_File : Boolean := False) return Profile_Type;
   --  Return the local profile. If the profile file read is not correct, a
   --  Participant_Error exception is raised.

   procedure Save_Local_Profile (Local_Profile : Profile_Type);
   --  Write the local profile to the appropriate file.
   --  If the file already exists, it is overwritten.
   --  If there are permission issues or the file cannot be written, the
   --  Participant_Error exception is raised.
   --  If the profile is invalid (e.g., missing required fields), the
   --  Participant_Error exception is raised.

   procedure Load_Local_Profile_Into (Remote_Profile : in out Profile_Type);
   --  Load the local profile into the given Remote_Profile. This operation
   --  overwrites any existing data in the `Remote_Profile` parameter with the
   --  data from the local profile.

   procedure Load_Profile_From_Token
     (Profile : in out Profile_Type; Token : String);
   --  Load the user profile from the GitHub token. This operation overwrites
   --  any existing data in the `Profile` parameter with the data from the
   --  GitHub token (except the SPDX license ID, if the local profile already
   --  exists).
   --
   --  If the local profile exists, the token is used to update the local
   --  profile only if it pertains to the same GitHub login account, otherwise
   --  an error is shown.
   --
   --  If the local profile does not exist, the token is used to create a new
   --  local profile. The token is used to retrieve the user information from
   --  the GitHub API. The user information is used to populate the local
   --  profile.
   --
   --  If an exception is raised during this operation, the `Profile` parameter
   --  is left in an undefined state. It is the caller's responsibility to ensure
   --  that the `Profile` parameter is not used in such cases without reinitialization.
   --
   --  If the token is invalid or expired, the procedure raises the
   --  Participant_Error exception. Ensure that the token is valid before
   --  calling this procedure.

   procedure Show (Profile : Profile_Type);
   --  Write the profile contents to the console; hides the token, if present.

end Alice_Participant;
