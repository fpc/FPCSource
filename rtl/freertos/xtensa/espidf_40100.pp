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
unit espidf_40100;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$linklib esp32,static}
{$linklib soc,static}
{$linklib driver,static}
{$linklib freertos,static}
{$linklib log,static}
{$linklib esp_common,static}
{$linklib heap,static}
{$linklib newlib,static}
{$linklib vfs,static}
{$linklib esp_ringbuf,static}
{$linklib spi_flash,static}
{$linklib app_update,static}
{$linklib xtensa,static}
{$linklib bootloader_support,static}
{$linklib pthread,static}
{$linklib hal,static}
{$linklib libm,static}
{$linklib libg,static}
{$linklib c,static}
{$linklib esp_event,static}

implementation

end.
