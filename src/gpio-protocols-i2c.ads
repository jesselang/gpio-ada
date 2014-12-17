--    GPIO - A GPIO interface.
--    Copyright (C) 2014 Jesse Lang <jesselang.com>
--    See the bottom of this file for licensing details.
pragma License (Modified_GPL);

with Ada.IO_Exceptions;

generic -- GPIO.Protocols.I2C
   Clock_Signal : Signal_Handle;
   Data_Signal  : Signal_Handle;
package GPIO.Protocols.I2C is
   procedure Start;

   procedure Stop;

   type Device_Address is range 0 .. 127;
   for Device_Address'Size use 7;

   type Byte is mod 2 ** 8;
   for Byte'Size use 8;

   type Byte_Count is range 0 .. 1023;
   subtype Byte_Index is Byte_Count range 1 .. Byte_Count'Last;

   type Byte_List is array (Byte_Index range <>) of Byte;
   pragma Pack (Byte_List);

   Device_Error : exception renames Ada.IO_Exceptions.Device_Error;

   procedure Write (Address : in Device_Address; Data : in Byte_List);

   procedure Read (Address : in Device_Address; Data : out Byte_List; Last : out Byte_Count);
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
