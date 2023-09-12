-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with GitHub.Profile; use GitHub.Profile;

package Alice_Participant is

   package PFile renames GitHub.Profile;

   Participant_Error : exception;

   function Has_Profile return Boolean;
   --  Check if the user profile file exists.

   procedure Load_Profile (Profile : in out Profile_Type);
   --  Read the user profile from the user profile file.

   procedure Save_Profile (Profile : Profile_Type);
   --  Write the user profile to the user profile file.

   procedure Set_Profile_From_Token
     (Profile : in out Profile_Type; Token : String);

   function Get_Current_Participant return Profile_Type;
   --  Return the currently configured user, reading the profile from file
   --  when needed.

   procedure Show (Profile : Profile_Type);
   --  Write the user profile contents to the console.

end Alice_Participant;
