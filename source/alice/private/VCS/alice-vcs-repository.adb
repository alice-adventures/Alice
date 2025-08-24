-------------------------------------------------------------------------------
--
--  ALICE - Adventures for Learning and Inspiring Coding Excellence
--  Copyright (c) 2023-2025 Francesc Rocher <francesc.rocher@gmail.com>
--  SPDX-License-Identifier: MIT
--
-------------------------------------------------------------------------------

with Alice.Context;
with Alice.Env;
with Alice.Hint;
with Alice.IFace.OS_Cmd;
with Alice.Std;
with Alice.VCS.Repository.Git;

package body Alice.VCS.Repository is

   package body Result is

      ----------------------
      -- Put_Image_Object --
      ----------------------

      procedure Put_Image_Object
        (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
         Self   : Object) is
      begin
         Output.Put ("([" & Self'Address'Image & " ] with");
         Alice.Env.Increase_Indent (Output);

         Output.New_Line;
         Output.Put ("Status  => " & Self.Status'Image);
         Output.New_Line;

         case Self.Status is
            when Alice.Result.Success =>
               Output.Put ("Repository => " & Self.Repository'Image);

            when Alice.Result.Error =>
               Output.Put ("Level   => " & Self.Level'Image);
               Output.New_Line;
               Output.Put ("Message => " & Self.Message'Image);
               Output.New_Line;
               Output.Put ("Hint    => " & Self.Hint'Image);
               Output.New_Line;
         end case;

         Alice.Env.Decrease_Indent (Output);
         Output.Put (")");
      end Put_Image_Object;

   end Result;

   -----------------------------
   -- CWD_Is_A_Git_Repository --
   -----------------------------

   function CWD_Is_A_Git_Repository return Boolean is
      Context : constant Alice.Context.Object_Access := Alice.Std.Get_Context;
   begin
      Result : constant Alice.IFace.OS_Cmd.Result_Exit'Class :=
        Context.OS_Cmd.Git.all.Run ("rev-parse --git-dir");

      return
        Result.Status = Alice.Result.Success and then Result.Exit_Status = 0;
   end CWD_Is_A_Git_Repository;

   -----------------------------
   -- Get_Repository_From_CWD --
   -----------------------------

   function Get_Repository_From_CWD return Alice.VCS.Repository.Result.Object
   is
   begin
      if CWD_Is_A_Git_Repository then
         declare
            Result : constant Alice.VCS.Repository.Result.Object :=
              (Alice.Controlled
               with
                 Status     => Alice.Result.Success,
                 Repository => new Alice.VCS.Repository.Git.Object);
         begin
            return Result;
         end;
      else
         declare
            Result : constant Alice.VCS.Repository.Result.Object :=
              (Alice.Controlled
               with
                 Status  => Alice.Result.Error,
                 Level   => Alice.Result.Domain,
                 Message => Alice.UStr ("CWD is not a VCS repository"),
                 Hint    => Alice.Hint.None);
         begin
            return Result;
         end;
      end if;
   --  #TODO - Implement logic to detect the type of VCS in the current
   --  working directory (e.g., Git, SVN, Mercurial) and return the
   --  corresponding repository object. For now, we only support and assume
   --  it's always Git.
   end Get_Repository_From_CWD;

end Alice.VCS.Repository;
