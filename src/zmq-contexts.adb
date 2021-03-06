-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                          Z M Q . C O N T E X T S                          --
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

with Interfaces.C;
with ZMQ.Low_Level;
with GNAT.OS_Lib;
with GNAT.Source_Info;

package body ZMQ.Contexts is

   use Interfaces.C;
   use System;

   ----------------
   -- Initialize --
   ----------------

   not overriding procedure Initialize
     (This       : in out Context;
      IO_Threads : Natural)
   is
   begin
      Validate_Library_Version;
      if This.ctx /= Null_Address then
         raise ZMQ_Error with "Already Initialized";
      end if;

      This.ctx := Low_Level.zmq_init (int (IO_Threads));

      if This.ctx = Null_Address then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
         GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (This : in out Context)
   is
      rc : int;
   begin
      if This.Is_Connected then
         rc := Low_Level.zmq_term (This.ctx);
         This.ctx := Null_Address;
         if rc /= 0 then
            raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
            GNAT.Source_Info.Enclosing_Entity;
         end if;
      end if;
   end Finalize;

   function Is_Connected (This : Context) return Boolean is
   begin
      return This.ctx /= Null_Address;
   end Is_Connected;

   function Intrinsic (This : Context) return System.Address is
   begin
      return This.ctx;
   end Intrinsic;

end ZMQ.Contexts;
