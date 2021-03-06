-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                                   Z M Q                                   --
--                                                                           --
--                                  B o d y                                  --
--                                                                           --
--            Copyright (C) 2010-2011, per.sandberg@bredband.net             --
--                                                                           --
--  Permission is hereby granted, free of charge, to any person obtaining a  --
--  copy of this software and associated documentation files                 --
--  (the "Software"), to deal in the Software without restriction, including --
--  without limitation the rights to use, copy, modify, merge, publish,      --
--  distribute, sublicense, and / or sell copies of the Software, and to     --
--  permit persons to whom the Software is furnished to do so, subject to    --
--  the following conditions :                                               --
--                                                                           --
--  The above copyright notice and this permission notice shall be included  --
--  in all copies or substantial portions of the Software.                   --
--                                                                           --
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  --
--  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               --
--  MERCHANTABILITY,                                                         --
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL  --
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR     --
--  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,    --
--  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR    --
--  OTHER DEALINGS IN THE SOFTWARE.                                          --
-------------------------------------------------------------------------------


with Ada.Assertions;
with Interfaces.C.Strings;

private with ZMQ.Low_Level;

package body ZMQ is

   use Interfaces.C;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message (no : Integer) return String is
      s : constant String := no'Img;
   begin
      return "[" &  s (s'First + 1 .. s'Last) & "] " &
      Interfaces.C.Strings.Value (Low_Level.zmq_strerror (int (no)));
   end Error_Message;

   function Library_Version return Version_Type is
      Major : aliased int;
      Minor : aliased int;
      Patch : aliased int;
   begin
      return ret : Version_Type do
         Low_Level.zmq_version (Major'Access,
                                Minor'Access,
                                Patch'Access);
         ret := (Natural (Major), Natural (Minor), Natural (Patch));
      end return;
   end Library_Version;

   function Image (Item : Version_Type) return String is
      S1 : constant String := Item.Major'Img;
      S2 : constant String := Item.Minor'Img;
      S3 : constant String := Item.Patch'Img;
   begin
      return
        S1 (S1'First + 1 .. S1'Last) & '.' &
        S2 (S2'First + 1 .. S2'Last) & '.' &
        S3 (S3'First + 1 .. S3'Last);
   end Image;

   procedure Validate_Library_Version is
      Lib_Version : constant Version_Type := Library_Version;
   begin
      Ada.Assertions.Assert
        ((Binding_Version.Major = Lib_Version.Major) and
           (Binding_Version.Minor = Lib_Version.Minor),
         "Incompatible Library: " & Image (Lib_Version));
   end Validate_Library_Version;

end ZMQ;
