--    GPIO - A GPIO interface.
--    Copyright (C) 2014 Jesse Lang <jesselang.com>
--    See the bottom of this file for licensing details.
pragma License (Modified_GPL);

with Ada.Characters.Handling;
with Ada.Strings;
with SysFS.Utilities;

use SysFS;

package body GPIO.SysFS is
   GPIO_Path : constant String := Utilities.Mount_Point & "/class/gpio";

   overriding
   procedure Get (Signal : in out SysFS_Signal; Power : out Signal_Power)is
   begin -- Get
      Get_Value (Signal => Signal, Result => Power);
   end Get;

   overriding
   procedure Set (Signal : in out SysFS_Signal; Power : in Signal_Power) is
   begin -- Set
      Set_Value (Signal => Signal, Power => Power);
   end Set;

   function Image (Number : Signal_Index) return String;

   GPIO_Signal_Prefix : constant String := GPIO_Path & "/gpio";

   procedure Export (Signal : out SysFS_Signal; Number : in Signal_Index; Initial : in Initial_Info) is
      Export_Control : Attributes.Attribute_Info;
   begin -- Export
      Attributes.Set_Path (Attribute => Export_Control, Path => GPIO_Path & "/export");
      Attributes.Write    (Attribute => Export_Control, Item => Image (Number) );

      delay 0.1; -- Wait for the GPIO signal attributes to populate.

      Signal.Number := Number;
      Attributes.Set_Path
         (Attribute => Signal.Direction_Control, Path => GPIO_Signal_Prefix & Image (Signal.Number) & "/direction");
      Attributes.Set_Path (Attribute => Signal.Value_Control,     Path => GPIO_Signal_Prefix & Image (Signal.Number) & "/value");
      Signal.Set_Initial_Direction (Initial => Initial);
   end Export;

   procedure Unexport (Signal : in out SysFS_Signal) is
      Unexport_Control : Attributes.Attribute_Info;
   begin -- Unexport
      Attributes.Set_Path (Attribute => Unexport_Control, Path => GPIO_Path & "/unexport");
      Attributes.Write    (Attribute => Unexport_Control, Item => Image (Signal.Number) );
   end Unexport;

   procedure Get_Direction (Signal : in out SysFS_Signal; Result : out Signal_Direction) is
      Buffer : String (1 .. 5);
      Last   : Natural;
   begin -- Get_Direction
      Attributes.Read (Attribute => Signal.Direction_Control, Item => Buffer, Last => Last);

      if Buffer (Buffer'First .. Last) = "in" then
         Result := In_Signal;
      elsif Buffer (Buffer'First .. Last) = "out" then
         Result := Out_Signal;
      else
         raise Constraint_Error;
      end if;

      Signal.Direction := Result;
   end Get_Direction;

   procedure Set_Direction (Signal : in out SysFS_Signal; Direction : in Signal_Direction) is
   begin -- Set_Direction
      if Signal.Direction = Direction then
         return;
      end if;

      case Direction is
         when In_Signal =>
            Attributes.Write (Attribute => Signal.Direction_Control, Item => "in");
         when Out_Signal =>
            Attributes.Write (Attribute => Signal.Direction_Control, Item => "out");
      end case;

      Signal.Direction := Direction;
   end Set_Direction;

   procedure Set_Initial_Direction (Signal : in out SysFS_Signal; Initial : in Initial_Info) is
   begin -- Set_Initial_Direction
      case Initial.Direction is
         when In_Signal =>
            Attributes.Write (Attribute => Signal.Direction_Control, Item => "in");
         when Out_Signal =>
            Attributes.Write (Attribute => Signal.Direction_Control,
                              Item      => Ada.Characters.Handling.To_Lower (Signal_Power'Image (Initial.Power) ) );
      end case;

      Signal.Direction := Initial.Direction;
   end Set_Initial_Direction;

   procedure Get_Value (Signal : in out SysFS_Signal; Result : out Signal_Power) is
      Buffer : String (1 .. 2);
      Last   : Natural;
   begin -- Get_Value
      Signal.Set_Direction (Direction => In_Signal);

      Attributes.Read (Attribute => Signal.Value_Control, Item => Buffer, Last => Last);

      if Buffer (Buffer'First .. Last) = "0" then
         Result := Low;
      elsif Buffer (Buffer'First .. Last) = "1" then
         Result := High;
      else
         raise Constraint_Error;
      end if;
   end Get_Value;

   procedure Set_Value (Signal : in out SysFS_Signal; Power : in Signal_Power) is
   begin -- Set_Value
      Signal.Set_Direction (Direction => Out_Signal);

      case Power is
         when Low =>
            Attributes.Write (Attribute => Signal.Value_Control, Item => "0");
         when High =>
            Attributes.Write (Attribute => Signal.Value_Control, Item => "1");
      end case;
   end Set_Value;

   function Image (Number : Signal_Index) return String is
      Number_Image : constant String := Signal_Index'Image (Number);
   begin -- Image
      if Number_Image (1) = Ada.Strings.Space then
         return Number_Image (2 .. Number_Image'Last);
      else
         return Number_Image;
      end if;
   end Image;
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
