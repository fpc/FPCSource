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
unit esp32c3idf_50000;

interface

{$linklib app_trace.a,static}
{$linklib app_update.a,static}
{$linklib bootloader_support.a,static}
{$linklib cmock.a,static}
{$linklib console.a,static}
{$linklib core.a,static}
{$linklib cxx.a,static}
{$linklib driver.a,static}
{$linklib efuse.a,static}
{$linklib esp_adc.a,static}
{$linklib esp_app_format.a,static}
{$linklib esp_coex.a,static}
{$linklib esp_common.a,static}
{$linklib espcoredump.a,static}
{$linklib esp_eth.a,static}
{$linklib esp_event.a,static}
{$linklib esp_gdbstub.a,static}
{$linklib esp_hid.a,static}
{$linklib esp_http_client.a,static}
{$linklib esp_http_server.a,static}
{$linklib esp_https_ota.a,static}
{$linklib esp_hw_support.a,static}
{$linklib esp_lcd.a,static}
{$linklib esp_local_ctrl.a,static}
{$linklib esp_mm.a,static}
{$linklib esp_netif.a,static}
{$linklib espnow.a,static}
{$linklib esp_partition.a,static}
{$linklib esp_phy.a,static}
{$linklib esp_pm.a,static}
{$linklib esp_ringbuf.a,static}
{$linklib esp_rom.a,static}
{$linklib esp_system.a,static}
{$linklib esp_timer.a,static}
{$linklib esp-tls.a,static}
{$linklib esp_wifi.a,static}
{$linklib fatfs.a,static}
{$linklib freertos.a,static}
{$linklib hal.a,static}
{$linklib heap.a,static}
{$linklib http_parser.a,static}
{$linklib json.a,static}
{$linklib log.a,static}
{$linklib lwip.a,static}
{$linklib main.a,static}
{$linklib mbedcrypto.a,static}
{$linklib mbedtls.a,static}
{$linklib mbedx509.a,static}
{$linklib mesh.a,static}
{$linklib mqtt.a,static}
{$linklib net80211.a,static}
{$linklib newlib.a,static}
{$linklib nvs_flash.a,static}
{$linklib pp.a,static}
{$linklib protobuf-c.a,static}
{$linklib protocomm.a,static}
{$linklib pthread.a,static}
{$linklib riscv.a,static}
{$linklib sdmmc.a,static}
{$linklib smartconfig.a,static}
{$linklib soc.a,static}
{$linklib spiffs.a,static}
{$linklib spi_flash.a,static}
{$linklib tcp_transport.a,static}
{$linklib unity.a,static}
{$linklib vfs.a,static}
{$linklib wapi.a,static}
{$linklib wear_levelling.a,static}
{$linklib wifi_provisioning.a,static}
{$linklib wpa_supplicant.a,static}

implementation

end.
