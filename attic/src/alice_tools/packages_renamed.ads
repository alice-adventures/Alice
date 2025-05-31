with Ada.Directories;

with Simple_Logging;

with Alice_Environment;
with Alice_Participant;

package Packages_Renamed is

   package Dir renames Ada.Directories;
   package Env renames Alice_Environment;
   package Log renames Simple_Logging;
   package Usr renames Alice_Participant;

end Packages_Renamed;
