package Alice_Env is

   function Is_Alice_Repository
     (Report_Error : Boolean := True) return Boolean;
   --  Check if the current working directory belongs to the Alice
   --  repository.

   function Is_Alice_Root_Dir (Report_Error : Boolean := True) return Boolean;
   --  Check if the current working directory belongs to the Alice repository
   --  and it is the root directory.

end Alice_Env;
