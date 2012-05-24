-------------------------------------------------------------------------------
--                   Copyright (c) 2011 Per Sandberg                         --
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


with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;
with GNAT.Source_Info;

package body ZMQ.Pollsets_Generic is

   use Interfaces.C;

   ----------------
   -- Initialize --
   ----------------

   not overriding
   procedure Initialize
     (This  : in out Pollsets;
      Using : in Poll_Item_Array)
   is
   begin
      Finalize (This);

      This.Set := new zmq_pollitem_t_Array (Using'Range);
      This.Count := 0;

      for Index in Using'Range loop
         declare
            Poll_Item : Poll_Items renames Using (Index);
         begin
            This.Set (Index) :=
              (socket  => Poll_Item.Socket.Intrinsic,
               fd      => 0,
               events  => short (Poll_Item.Events),
               revents => 0);
         end;
      end loop;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (This : in out Pollsets)
   is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Object => zmq_pollitem_t_Array,
           Name   => zmq_pollitem_t_Array_Access);
   begin
      if This.Set /= null then
         Free (This.Set);
      end if;
   end Finalize;

   ----------
   -- Poll --
   ----------

   procedure Poll (This : in out Pollsets; Timeout_In_Microseconds : in long)
   is
   begin
      if This.Set = null then
         raise ZMQ_Error with
           GNAT.Source_Info.Enclosing_Entity & ": Pollset Uninitialized";
      end if;

      This.Count := Low_Level.zmq_poll
        (items   => This.Set (This.Set.all'First)'Unrestricted_Access,
         nitems  => This.Set.all'Length,
         timeout => Timeout_In_Microseconds);

      if This.Count < 0 then
         raise ZMQ_Error with
           Error_Message (GNAT.OS_Lib.Errno) & " in " &
           GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Poll;

   procedure Poll (This : in out Pollsets) is
   begin
      Poll (This, Timeout_In_Microseconds => -1);
   end Poll;

   procedure Poll (This : in out Pollsets; Timeout : in Duration)
   is
      Microsecond : constant Duration := 1.0e-6;
   begin
      Poll (This, Timeout_In_Microseconds => long (Timeout / Microsecond));
   end Poll;

   ------------------------
   -- Polled_Event_Count --
   ------------------------

   function Polled_Event_Count (This : in Pollsets) return Natural is
   begin
      if This.Set = null then
         raise ZMQ_Error with
           GNAT.Source_Info.Enclosing_Entity & ": Pollset Uninitialized";
      elsif This.Count < 0 then
         raise ZMQ_Error with
           GNAT.Source_Info.Enclosing_Entity &
           ": Pollset invalid due to unsuccessful poll";
      end if;

      return Natural (This.Count);
   end Polled_Event_Count;

   ----------------------
   -- Has_Polled_Event --
   ----------------------

   function Has_Polled_Event
     (This  : in Pollsets;
      Index : in Set_Indices;
      Event : in Event_Flags := Poll_In)
      return Boolean
   is
   begin
      if This.Set = null then
         raise ZMQ_Error with
           GNAT.Source_Info.Enclosing_Entity & ": Pollset Uninitialized";
      elsif This.Count < 0 then
         raise ZMQ_Error with
           GNAT.Source_Info.Enclosing_Entity &
           ": Pollset invalid due to unsuccessful poll";
      elsif Index not in This.Set.all'Range then
         raise ZMQ_Error with
           GNAT.Source_Info.Enclosing_Entity & ": Index out of Pollset bounds";
      end if;

      return (Event_Flags (This.Set (Index).revents) and Event) /= 0;
   end Has_Polled_Event;

end ZMQ.Pollsets_Generic;
