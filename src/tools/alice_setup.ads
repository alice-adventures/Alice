package Alice_Setup is

   function Initialize return Boolean;
   function Check_Status return Boolean;

private

   function Is_Alice_Repository return Boolean is (True);
   function Is_Alice_Root_Dir return Boolean is (True);

end Alice_Setup;
