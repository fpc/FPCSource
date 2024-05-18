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
unit esp32c3idf_50000;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$linklib app_update,static}
{$linklib bootloader_support,static}
{$linklib core,static}
{$linklib driver,static}
{$linklib efuse,static}
{$linklib esp_app_format,static}
{$linklib esp_common,static}
{$linklib esp_hw_support,static}
{$linklib esp_partition,static}
{$linklib esp_pm,static}
{$linklib esp_ringbuf,static}
{$linklib esp_rom,static}
{$linklib esp_system,static}
{$linklib esp_timer,static}
{$linklib freertos,static}
{$linklib hal,static}
{$linklib heap,static}
{$linklib log,static}
{$linklib newlib,static}
{$linklib pthread,static}
{$linklib riscv,static}
{$linklib soc,static}
{$linklib spi_flash,static}
{$linklib vfs,static}
{$linklib c,static}
{$linklib m,static}
{$linklib gcc,static}

implementation

end.
