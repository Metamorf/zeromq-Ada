-------------------------------------------------------------------------------
--                                                                           --
--                             0MQ Ada-binding                               --
--                                                                           --
--                          Z M Q . M E S S A G E S                          --
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
with GNAT.OS_Lib;
with GNAT.Source_Info;
with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;

package body ZMQ.Messages is
   use Interfaces.C;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Message) is
      ret : int;
   begin
      ret := Low_Level.zmq_msg_init (This.Msg'Access);
      if ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
         GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Initialize;

   --  ========================================================================
   --  Initialize with size
   --  ========================================================================

   procedure Initialize (This : in out Message; Size : Natural) is
      ret : int;
   begin
      ret := Low_Level.zmq_msg_init_size (This.Msg'Access, size_t (Size));
      if ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
         GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Initialize;

   procedure Initialize
     (This : in out Message;
      Size : Ada.Streams.Stream_Element_Offset)
   is
   begin
      This.Initialize (Natural (Size));
   end Initialize;

   --  ========================================================================
   --  Initialize with data
   --  ========================================================================
   procedure Initialize (This : in out Message;
                         Data : System.Address;
                         Size : Natural)
   is
      type Data_Type is
        new Ada.Streams.Stream_Element_Array
          (1 .. Ada.Streams.Stream_Element_Count (Size));
      package Convert is
        new System.Address_To_Access_Conversions (Data_Type);
   begin
      This.Initialize (Size);
      Convert.To_Pointer (This.getData).all := Convert.To_Pointer (Data).all;
   end Initialize;

   procedure Initialize (This : in out Message; Data : String) is
   begin
      This.Initialize (Data'Address, Data'Length);
   end Initialize;

   procedure Initialize
     (This : in out Message;
      Data : Ada.Streams.Stream_Element_Array) is
   begin
      This.Initialize (Data'Address, Data'Length);
   end Initialize;

   --  ========================================================================
   --  Initialize with data ref and free
   --  ========================================================================
   procedure Initialize
     (This    : in out Message;
      Message : System.Address;
      Size    : Natural;
      Free    : Free_Proc;
      Hint    : System.Address := System.Null_Address)
   is
      ret : int;
   begin
      ret := Low_Level.zmq_msg_init_data (This.Msg'Access,
                                          Message,
                                          size_t (Size),
                                          Free,
                                          Hint);
      if ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
         GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Initialize;


   ------------------------
   -- Initialize_Generic --
   ------------------------

   procedure Initialize_Generic
     (This   : in out Message;
      Data   : Element_Access)
   is
      function conv is new
        Ada.Unchecked_Conversion (Source => System.Address,
                                  Target => Element_Access);
      procedure Internal_free (Data : System.Address; Hint : System.Address);
      procedure Internal_free (Data : System.Address; Hint : System.Address) is
         pragma Unreferenced (Hint);
         temp  : Element_Access := conv (Data);
      begin
         free (temp);
      end Internal_free;
   begin
      This.Initialize (Data.all'Address,
                       Data.all'Size / 8,
                       Internal_free'Unrestricted_Access);
   end Initialize_Generic;

   ----------------------------------
   -- Initialize_Generic_With_Hint --
   ----------------------------------

   procedure Initialize_Generic_With_Hint
     (This  : in out Message;
      Data  : Element_Access;
      Hint  : Hint_Access)
   is
      function convHint is new
        Ada.Unchecked_Conversion (Source => System.Address,
                                  Target => Hint_Access);
      function conv is new
        Ada.Unchecked_Conversion (Source => System.Address,
                                  Target => Element_Access);

      procedure Internal_free (Data : System.Address; Hint : System.Address);
      procedure Internal_free (Data : System.Address; Hint : System.Address) is
         Temp_Data  : Element_Access := conv (Data);
         Temp_Hint  : constant Hint_Access := convHint (Hint);
      begin
         free (Temp_Data, Temp_Hint);
      end Internal_free;
   begin
      This.Initialize (Data.all'Address,
                       Data.all'Size / 8,
                       Internal_free'Unrestricted_Access,
                       Hint.all'Address);
   end Initialize_Generic_With_Hint;


   --  ========================================================================
   --  GetData / GetSize
   --  ========================================================================

   ---------------
   --  getData  --
   ---------------
   function getData (This : Message) return System.Address is
   begin
      return Low_Level.zmq_msg_data (This.Msg'Unrestricted_Access);
   end getData;


   ---------------
   --  getData  --
   ---------------
   function getData (This : Message) return String is
      type data_type is new String (1 .. This.getSize);
      package conv is new System.Address_To_Access_Conversions (data_type);
   begin
      return String (conv.To_Pointer (This.getData).all);
   end getData;

   ---------------
   --  getData  --
   ---------------
   function  getData  (This : Message)
                       return Ada.Streams.Stream_Element_Array is
      type data_type is new Ada.Streams.Stream_Element_Array
        (1 .. Ada.Streams.Stream_Element_Offset (This.getSize));
      package conv is new System.Address_To_Access_Conversions (data_type);
   begin
      return Ada.Streams.Stream_Element_Array
        (conv.To_Pointer (This.getData).all);
   end getData;

   ---------------
   --  getData  --
   ---------------
   function  getData  (This : Message)
                       return Ada.Strings.Unbounded.Unbounded_String is
      type data_type is new String (1 .. This.getSize);
      package conv is new System.Address_To_Access_Conversions (data_type);
   begin
      return Ada.Strings.Unbounded.To_Unbounded_String
        (String (conv.To_Pointer (This.getData).all));
   end getData;

   -----------------------
   --  getData_Generic  --
   -----------------------
   function  getData_Generic  (This : Message)
                               return Element is
      package conv is new System.Address_To_Access_Conversions (Element);
   begin
      return conv.To_Pointer (This.getData).all;
   end getData_Generic;


   ---------------
   --  getSize  --
   ---------------
   function getSize (This : Message) return Natural is
   begin
      return Natural (Low_Level.zmq_msg_size (This.Msg'Unrestricted_Access));
   end getSize;


   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Message) is
      ret : int;
   begin
      ret := Low_Level.zmq_msg_close (This.Msg'Access);
      if ret /= 0 then
         raise ZMQ_Error with Error_Message (GNAT.OS_Lib.Errno) & " in " &
         GNAT.Source_Info.Enclosing_Entity;
      end if;
   end Finalize;


   procedure process_data_generic
     (This   : Message;
      Handle : access procedure (item : Element)) is
      package conv is new System.Address_To_Access_Conversions (Element);
   begin
      Handle (conv.To_Pointer (This.getData).all);
   end process_data_generic;

   ---------------
   -- Intrinsic --
   ---------------

   function Intrinsic (This : Message)
                       return not null access ZMQ.Low_Level.zmq_msg_t is
   begin
      return This.Msg'Unrestricted_Access;
   end Intrinsic;

end ZMQ.Messages;
