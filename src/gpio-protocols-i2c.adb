--    GPIO - A GPIO interface.
--    Copyright (C) 2014 Jesse Lang <jesselang.com>
--    See the bottom of this file for licensing details.
pragma License (Modified_GPL);

with Ada.Real_Time;
with System;

package body GPIO.Protocols.I2C is
   Bit_Rate   : constant                         := 100_000; -- Bits per second.
   Half_Cycle : constant Ada.Real_Time.Time_Span := Ada.Real_Time.To_Time_Span (0.5 / Duration (Bit_Rate) );

   Next_Change : Ada.Real_Time.Time := Ada.Real_Time.Time_First;
   -- pragma Atomic (Next_Change); -- Can't be guaranteed on FSF GNAT 4.6 on ARM.

   Started : Boolean := False;
   -- pragma Atomic (Started);     -- Can't be guaranteed on FSF GNAT 4.6 on ARM.

   use type Ada.Real_Time.Time;

   procedure Start is
      use type Ada.Real_Time.Time_Span;
   begin -- Start
      if Started then
         -- Repeated start.
         delay until Next_Change - Half_Cycle / 2;

         Data_Signal.Set (Power => High);

         delay until Next_Change;

         Next_Change := Next_Change + Half_Cycle;
         Clock_Signal.Set (Power => High);

         delay until Next_Change - Half_Cycle / 2;

         Data_Signal.Set (Power => Low);

         delay until Next_Change;

         Next_Change := Next_Change + Half_Cycle;
         Clock_Signal.Set (Power => Low);
      else
         if Next_Change = Ada.Real_Time.Time_First then
            Next_Change := Ada.Real_Time.Clock + Half_Cycle;
         else
            Next_Change := Next_Change + Half_Cycle;
         end if;

         Data_Signal.Set (Power => Low);

         delay until Next_Change;

         Next_Change := Next_Change + Half_Cycle;
         Clock_Signal.Set (Power => Low);
      end if;

      Started := True;
   end Start;

   procedure Stop is
   begin -- Stop
      delay until Next_Change;

      Next_Change := Next_Change + Half_Cycle;
      Clock_Signal.Set (Power => High);

      delay until Next_Change;

      Next_Change := Next_Change + Half_Cycle;
      Data_Signal.Set (Power => High);
      Started := False;
   end Stop;

   subtype Bit is Signal_Power;

   type Byte_Frame is array (1 .. 8) of Bit;
   pragma Pack (Byte_Frame);

   procedure Write (Frame : in Byte_Frame);

   type Request_Mode is (Write, Read);
   for Request_Mode'Size use 1;
   for Request_Mode use
      (Write => 0,
       Read  => 1);

   type Address_Frame is record
      Address : Device_Address;
      Control : Request_Mode;
   end record;
   pragma Pack (Address_Frame);

   type Byte_Frame_List is array (Byte_Count range <>) of Byte_Frame;
   pragma Pack (Byte_Frame_List);

   procedure Write (Address : in Address_Frame);

   procedure Write (Address : in Device_Address; Data : in Byte_List) is
      Data_Overlay : Byte_Frame_List (Data'Range);
      for Data_Overlay'Address use Data'Address;
   begin -- Write
      Write (Address => (Address => Address, Control => Write) );

      for I in Data_Overlay'Range loop
         Write (Frame => Data_Overlay (I) );
      end loop;
   end Write;

   procedure Read (Frame : out Byte_Frame; Continue : in Boolean := True);

   procedure Read (Address : in Device_Address; Data : out Byte_List; Last : out Byte_Count) is
      Data_Overlay : Byte_Frame_List (Data'Range);
      for Data_Overlay'Address use Data'Address;
   begin -- Read
      Write (Address => (Address => Address, Control => Read) );
      Last := 0;

      for I in Data_Overlay'Range loop
         Handle_Error : begin
            Read (Frame => Data_Overlay (I), Continue => I < Data_Overlay'Last);
         exception -- Handle_Error
            when Device_Error =>
               Last := I - 1;
         end Handle_Error;
      end loop;
   end Read;

   procedure Write (Item : in Bit);
   pragma Inline (Write);
   procedure Read (Item : out Bit);
   pragma Inline (Read);

   procedure Write (Address : in Address_Frame) is
      Address_Overlay : Byte_Frame;
      for Address_Overlay'Address use Address'Address;

      NACK : Bit;

      use type System.Bit_Order;
   begin -- Write
      if System.Default_Bit_Order = System.Low_Order_First then
         Address_Bits_Low : for I in reverse 1 .. Device_Address'Size loop
            Write (Item => Address_Overlay (I) );
         end loop Address_Bits_Low;
      else
         Address_Bits_High : for I in 1 .. Device_Address'Size loop
            Write (Item => Address_Overlay (I) );
         end loop Address_Bits_High;
      end if;

      Write (Item => Address_Overlay (Address_Overlay'Last) );
      Read (Item => NACK);

      if NACK = High then
         raise Device_Error;
      end if;
   end Write;

   procedure Write (Frame : in Byte_Frame) is
      NACK : Bit;

      use type System.Bit_Order;
   begin -- Write
      if System.Default_Bit_Order = System.Low_Order_First then
         Frame_Bits_Low : for I in reverse Frame'Range loop
            Write (Item => Frame (I) );
         end loop Frame_Bits_Low;
      else
         Frame_Bits_High : for I in Frame'Range loop
            Write (Item => Frame (I) );
         end loop Frame_Bits_High;
      end if;

      Read (Item => NACK);

      if NACK = High then
         raise Device_Error;
      end if;
   end Write;

   procedure Read (Frame : out Byte_Frame; Continue : in Boolean := True) is
      use type System.Bit_Order;
   begin -- Read
      if System.Default_Bit_Order = System.Low_Order_First then
         Frame_Bits_Low : for I in reverse Frame'Range loop
            Read (Item => Frame (I) );
         end loop Frame_Bits_Low;
      else
         Frame_Bits_High : for I in Frame'Range loop
            Read (Item => Frame (I) );
         end loop Frame_Bits_High;
      end if;

      if Continue then
         Write (Item => Low);
      else
         Write (Item => High);
      end if;
   end Read;

   procedure Write (Item : in Bit) is
      use type Ada.Real_Time.Time_Span;
   begin -- Write
      delay until Next_Change - Half_Cycle / 2;

      Data_Signal.Set (Power => Item);

      delay until Next_Change;

      Next_Change := Next_Change + Half_Cycle;
      Clock_Signal.Set (Power => High);

      delay until Next_Change;

      Next_Change := Next_Change + Half_Cycle;
      Clock_Signal.Set (Power => Low);
   end Write;

   procedure Read (Item : out Bit) is
      use type Ada.Real_Time.Time_Span;
   begin -- Read
      delay until Next_Change;

      Next_Change := Next_Change + Half_Cycle;
      Clock_Signal.Set (Power => High);

      delay until Next_Change - Half_Cycle / 2;

      Data_Signal.Get (Power => Item);

      delay until Next_Change;

      Next_Change := Next_Change + Half_Cycle;
      Clock_Signal.Set (Power => Low);
   end Read;
end GPIO.Protocols.I2C;

--    This program is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
--    As a special exception, if other files instantiate generics from
--    this unit, or you link this unit with other files to produce an
--    executable, this unit does not by itself cause the resulting
--    executable to be covered by the GNU General Public License. This
--    exception does not however invalidate any other reasons why the
--    executable file might be covered by the GNU Public License.
