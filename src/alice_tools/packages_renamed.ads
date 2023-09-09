
with Ada.Directories;

with Simple_Logging;

with Alice_Environment;
with Alice_User_Config;

package Packages_Renamed is

   package Dir renames Ada.Directories;
   package Env renames Alice_Environment;
   package Log renames Simple_Logging;
   package Usr renames Alice_User_Config;

end Packages_Renamed;
