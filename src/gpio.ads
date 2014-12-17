--    GPIO - A GPIO interface.
--    Copyright (C) 2014 Jesse Lang <jesselang.com>
--    See the bottom of this file for licensing details.
pragma License (Modified_GPL);

package GPIO is
   type Signal_Direction is (In_Signal, Out_Signal);
   type Signal_Power is (Low, High);
   for Signal_Power'Size use 1;
   for Signal_Power use
      (Low  => 0,
       High => 1);

   type Signal_Index is new Positive;

   type GPIO_Signal is abstract tagged limited private;
   type Signal_Handle is access all GPIO_Signal'Class;

   procedure Get (Signal : in out GPIO_Signal; Power : out Signal_Power) is abstract;

   procedure Set (Signal : in out GPIO_Signal; Power : in Signal_Power) is abstract;

   procedure Set_Initial_Power (Signal : in out GPIO_Signal; Power : in Signal_Power);
private -- GPIO
   type GPIO_Signal is abstract tagged limited record
      Initial_Power : Signal_Power := Low;
   end record;
end GPIO;

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
