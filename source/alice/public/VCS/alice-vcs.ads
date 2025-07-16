-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

package Alice.VCS is

   Key_Avatar_URL     : constant String := "avatar_url";
   Key_Email          : constant String := "email";
   Key_Login          : constant String := "login";
   Key_Name           : constant String := "name";
   Key_Service        : constant String := "service";
   Key_SPDX_Id        : constant String := "spdx_id";
   Key_Token          : constant String := "token";
   Key_Type           : constant String := "type";
   Key_User_View_Type : constant String := "user_view_type";
   --  Keys used in JSON and TOML files to extract profile information. These
   --  keys are used to access specific fields in theses files, such as the
   --  user's avatar URL, email, login name, full name, type (e.g., user or
   --  organization), and the view type of the user profile.

end Alice.VCS;
