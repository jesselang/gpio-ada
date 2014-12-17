--    GPIO - A GPIO interface.
--    Copyright (C) 2014 Jesse Lang <jesselang.com>
--    See the bottom of this file for licensing details.
pragma License (Modified_GPL);

private with SysFS.Attributes;

package GPIO.SysFS is
   type SysFS_Signal is new GPIO_Signal with private;

   overriding
   procedure Get (Signal : in out SysFS_Signal; Power : out Signal_Power);

   overriding
   procedure Set (Signal : in out SysFS_Signal; Power : in Signal_Power);

   type Initial_Info (Direction : Signal_Direction := In_Signal) is record
      case Direction is
         when In_Signal =>
            null;
         when Out_Signal =>
            Power : Signal_Power := Low;
      end case;
   end record;

   not overriding
   procedure Export (Signal : out SysFS_Signal; Number : in Signal_Index; Initial : in Initial_Info);

   not overriding
   procedure Unexport (Signal : in out SysFS_Signal);
private -- GPIO.SysFS
   type SysFS_Signal is new GPIO_Signal with record
      Number            : Signal_Index;
      Direction         : Signal_Direction;
      Value_Control     : Standard.SysFS.Attributes.Attribute_Info;
      Direction_Control : Standard.SysFS.Attributes.Attribute_Info;
   end record;

   not overriding
   procedure Get_Direction (Signal : in out SysFS_Signal; Result : out Signal_Direction);

   not overriding
   procedure Set_Direction (Signal : in out SysFS_Signal; Direction : in Signal_Direction);

   not overriding
   procedure Set_Initial_Direction (Signal : in out SysFS_Signal; Initial : in Initial_Info);

   not overriding
   procedure Get_Value (Signal : in out SysFS_Signal; Result : out Signal_Power);

   not overriding
   procedure Set_Value (Signal : in out SysFS_Signal; Power : in Signal_Power);
end GPIO.SysFS;

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
