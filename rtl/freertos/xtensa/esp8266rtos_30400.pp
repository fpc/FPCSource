{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2021 by Florian Klaempfl
    member of the Free Pascal development team.

    System unit for FreeRTOS systems

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit esp8266rtos_30400;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$linklib esp8266, static}
{$linklib log, static}
{$linklib newlib, static}
{$linklib c_nano, static}
{$linklib heap, static}
{$linklib vfs, static}
{$linklib esp_common, static}
{$linklib core, static}
{$linklib freertos, static}
{$linklib phy, static}
{$linklib net80211, static}
{$linklib hal, static}
{$linklib nvs_flash, static}
{$linklib rtc, static}
{$linklib spi_flash, static}
{$linklib esp_ringbuf, static}
{$linklib gcc, static}
{$linklib pp, static}
{$linklib stdc++, static}
{$linklib pthread, static}

implementation

end.
