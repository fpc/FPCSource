{
    Copyright (c) 1998-2022 by the Free Pascal development team

    Raspberry Pi 3 GPIO hardware defines for aarch64-embedded targets

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
{$IFNDEF FPC_DOTTEDUNITS}
unit gpio;
{$ENDIF FPC_DOTTEDUNITS}

interface

const
    PeripheralBase = $3F000000;

    GPFSEL0        = PeripheralBase + $00200000;
    GPFSEL1        = PeripheralBase + $00200004;
    GPFSEL2        = PeripheralBase + $00200008;
    GPFSEL3        = PeripheralBase + $0020000C;
    GPFSEL4        = PeripheralBase + $00200010;
    GPFSEL5        = PeripheralBase + $00200014;
    GPSET0         = PeripheralBase + $0020001C;
    GPSET1         = PeripheralBase + $00200020;
    GPCLR0         = PeripheralBase + $00200028;
    GPLEV0         = PeripheralBase + $00200034;
    GPLEV1         = PeripheralBase + $00200038;
    GPEDS0         = PeripheralBase + $00200040;
    GPEDS1         = PeripheralBase + $00200044;
    GPHEN0         = PeripheralBase + $00200064;
    GPHEN1         = PeripheralBase + $00200068;
    GPPUD          = PeripheralBase + $00200094;
    GPPUDCLK0      = PeripheralBase + $00200098;
    GPPUDCLK1      = PeripheralBase + $0020009C;

implementation

end.
