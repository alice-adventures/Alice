-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with AAA.Strings;
with AAA.Table_IO;
with AAA.Text_IO;
with Text_IO;

package body Alice_Cmd.PSource.List is

   overriding procedure Execute
     (Cmd : in out Cmd_Type; Args : AAA.Strings.Vector)
   is
      Table : AAA.Table_IO.Table;
   begin
      Text_IO.Put_Line ("Available Problem Sources");
      Text_IO.Put_Line ("");
      Table.Append ("  Tag").Append ("Name").Append ("URL").New_Row;
      Table.Append ("  ---").Append ("----").Append ("---").New_Row;
      for PSource of Available_PSources loop
         Table.Append ("  " & To_String (PSource.Tag)).Append
           (To_String (PSource.Name))
           .Append
           (To_String (PSource.URL))
           .New_Row;
      end loop;
      Table.Print ("    ");

      Text_IO.Put_Line ("");
      AAA.Text_IO.Put_Paragraph
        ("Note: when required, use the 'tag' in alice commands to refer to a specific Problem Source");
   end Execute;

end Alice_Cmd.PSource.List;
