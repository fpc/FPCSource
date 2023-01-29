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
unit espidf_40400;

interface

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
{$linklib esp_system, static}
{$linklib esp_hw_support, static}
{$linklib esp_rom, static}
{$linklib esp_timer, static}
{$linklib bootloader_support, static}
{$linklib esp_pm, static}
{$linklib driver, static}
{$linklib esp_ipc, static}
{$linklib xt_hal, static}
{$linklib efuse, static}

implementation

end.
