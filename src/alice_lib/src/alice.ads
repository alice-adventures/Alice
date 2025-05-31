with Simple_Logging;

package Alice is

   package Log renames Simple_Logging;

   function Version return String;
   --  Returns the version of the Alice library.

end Alice;
