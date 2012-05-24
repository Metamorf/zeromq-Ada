-------------------------------------------------------------------------------
--                   Copyright (c) 2012 Trevor Mettam                         --
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

with ZMQ.Sockets;
private with ZMQ.Low_Level;

private with Ada.Finalization;
private with Interfaces.C;

generic
   type Set_Indices is (<>);
package ZMQ.Pollsets_Generic is

   type Pollsets is tagged limited private;

   type Event_Flags is private;

   None     : constant Event_Flags;
   Poll_In  : constant Event_Flags;
   Poll_Out : constant Event_Flags;
   Poll_Err : constant Event_Flags;

   function "+" (Left, Right : Event_Flags) return Event_Flags;

   type Poll_Items is
      record
         Socket : Sockets.Socket;
         Events : Event_Flags;
      end record;

   type Poll_Item_Array is array (Set_Indices range <>) of Poll_Items;

   not overriding
   procedure Initialize (This  : in out Pollsets;
                         Using : in Poll_Item_Array);

   procedure Poll (This : in out Pollsets);

   procedure Poll (This    : in out Pollsets;
                   Timeout : in Duration);

   function Polled_Event_Count (This : in Pollsets) return Natural;

   function Has_Polled_Event (This  : in Pollsets;
                              Index : in Set_Indices;
                              Event : in Event_Flags := Poll_In)
                              return Boolean;

private

   type Event_Flags is mod 2 ** Interfaces.C.short'Size;

   None     : constant Event_Flags := 0;
   Poll_In  : constant Event_Flags := Low_Level.Defs.ZMQ_POLLIN;
   Poll_Out : constant Event_Flags := Low_Level.Defs.ZMQ_POLLOUT;
   Poll_Err : constant Event_Flags := Low_Level.Defs.ZMQ_POLLERR;

   function "+" (Left, Right : Event_Flags) return Event_Flags renames "or";

   type zmq_pollitem_t_Array is
     array (Set_Indices range <>) of aliased Low_Level.zmq_pollitem_t;

   type zmq_pollitem_t_Array_Access is access all zmq_pollitem_t_Array;

   type Pollsets is new Ada.Finalization.Limited_Controlled with
      record
         Set   : zmq_pollitem_t_Array_Access := null;
         Count : Interfaces.C.int := 0;
      end record;

   overriding
   procedure Finalize (This : in out Pollsets);

   procedure Poll (This                    : in out Pollsets;
                   Timeout_In_Microseconds : in Interfaces.C.long);

end ZMQ.Pollsets_Generic;
