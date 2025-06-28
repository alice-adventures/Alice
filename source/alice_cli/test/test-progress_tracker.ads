-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.IFace.Logger;
with Alice.IFace.Progress_Tracker;

package Test.Progress_Tracker is

   procedure Run
     (Log      : Alice.IFace.Logger.Object_Access;
      Progress : Alice.IFace.Progress_Tracker.Object_Access);

end Test.Progress_Tracker;
