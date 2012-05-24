-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                          Z M Q . M E S S A G E S                          --
--                                                                           --
--                                  S p e c                                  --
--                                                                           --
--            Copyright (C) 2010-2011, per.sandberg@bredband.net             --
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


with ZMQ.Low_Level;
with Ada.Streams;
with System;
with Ada.Strings.Unbounded;

package ZMQ.Messages is

   --  A �MQ message is a discrete unit of data passed between applications
   --  or components of the same application.
   --  �MQ messages have no internal structure and from the point of view
   --  of �MQ itself they are considered to be opaque binary data.


   type Message is tagged limited private;


   procedure Initialize (This : in out Message);
   --  Initialize empty 0MQ message
   --  Initialize the message object referenced by msg to represent
   --  an empty message.
   --  This function is most useful when called before receiving a message.

   procedure Initialize (This : in out Message; Size : Natural);
   procedure Initialize
     (This : in out Message; Size : Ada.Streams.Stream_Element_Offset);
   pragma Inline (Initialize);
   --  Initialize 0MQ message of a specified size
   --  Allocate resources required to store a message size bytes long
   --  and initialize the message object referenced by msg to represent
   --  the newly allocated message.
   --  The Allocated message area is not not cleared.

   procedure Initialize (This : in out Message;
                         Data : String);
   procedure Initialize (This : in out Message;
                         Data : Ada.Streams.Stream_Element_Array);
   pragma Inline (Initialize);
   procedure Initialize (This : in out Message;
                         Data : System.Address;
                         Size : Natural);
   pragma Inline (Initialize);
   --  Initialize 0MQ message of a specified size
   --  Allocate resources required to store the data
   --  and initialize the message object referenced by msg to represent
   --  the newly allocated message.
   --  The Data is copied into the message.


   type Free_Proc is not null access
     procedure (data : System.Address; Hint : System.Address);
   procedure Null_Free  (data : System.Address; Hint : System.Address) is null;
   procedure Initialize (This    : in out Message;
                         Message : System.Address;
                         Size    : Natural;
                         Free    : Free_Proc;
                         Hint    : System.Address := System.Null_Address);
   --  initialise 0MQ message from a supplied buffer


   generic
      type Element is private;
      type Element_Access is access Element;
      with procedure free (data : in out Element_Access) is <>;
   procedure Initialize_Generic (This   : in out Message;
                                 Data   : Element_Access);


   generic
      type Element is private;
      type Element_Access is access Element;
      type Hint_Type is private;
      type Hint_Access is access Hint_Type;
      with procedure free
        (data : in out Element_Access; hint : Hint_Access) is <>;
   procedure Initialize_Generic_With_Hint (This  : in out Message;
                                           Data  : Element_Access;
                                           Hint  : Hint_Access);


   procedure Finalize   (This : in out Message);


   function  getData  (This : Message)
                       return String;

   function  getData  (This : Message)
                       return Ada.Streams.Stream_Element_Array;

   function  getData  (This : Message)
                       return Ada.Strings.Unbounded.Unbounded_String;

   generic
      type Element is private;
   function  getData_Generic  (This : Message)
                               return Element;
   generic
      type Element is private;
   procedure process_data_generic
     (This   : Message;
      Handle : access procedure (item : in Element));

   function getData (This : Message) return System.Address;
   function getSize (This : Message) return Natural;


--     procedure Move (This : in out Message; To : in out Message);
--
--     procedure Copy (This : in Message; To : in out Message);


   function Intrinsic (This : Message)
                       return not null access ZMQ.Low_Level.zmq_msg_t;

private
   type Message is tagged limited record
      Msg : aliased ZMQ.Low_Level.zmq_msg_t;
   end record;

end ZMQ.Messages;
