{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

  Automatically converted by H2Pas 1.0.0 from power.h
  The following command line parameters were used:
    -d
    -c
    -w
    power.h
}

{ Power Management APIs }

{$ifdef read_interface}
const
   AC_LINE_OFFLINE = $00;   
   AC_LINE_ONLINE = $01;   
   AC_LINE_BACKUP_POWER = $02;   
   AC_LINE_UNKNOWN = $FF;   
   BATTERY_FLAG_HIGH = $01;
   BATTERY_FLAG_LOW = $02;   
   BATTERY_FLAG_CRITICAL = $04;   
   BATTERY_FLAG_CHARGING = $08;   
   BATTERY_FLAG_NO_BATTERY = $80;   
   BATTERY_FLAG_UNKNOWN = $FF;   
   BATTERY_PERCENTAGE_UNKNOWN = $FF;   
   BATTERY_LIFE_UNKNOWN = $FFFFFFFF;   
type

   _SYSTEM_POWER_STATUS_EX = record
        ACLineStatus : BYTE;
        BatteryFlag : BYTE;
        BatteryLifePercent : BYTE;
        Reserved1 : BYTE;
        BatteryLifeTime : DWORD;
        BatteryFullLifeTime : DWORD;
        Reserved2 : BYTE;
        BackupBatteryFlag : BYTE;
        BackupBatteryLifePercent : BYTE;
        Reserved3 : BYTE;
        BackupBatteryLifeTime : DWORD;
        BackupBatteryFullLifeTime : DWORD;
     end;
   SYSTEM_POWER_STATUS_EX = _SYSTEM_POWER_STATUS_EX;
   PSYSTEM_POWER_STATUS_EX = ^_SYSTEM_POWER_STATUS_EX;
   LPSYSTEM_POWER_STATUS_EX = ^_SYSTEM_POWER_STATUS_EX;
{
	@struct SYSTEM_POWER_STATUS_EX2 | contains information about the power status
	of the system.

    @field BYTE | ACLineStatus | AC power status. Must be one of
    AC_LINE_OFFLINE, AC_LINE_ONLINE, AC_LINE_BACKUP_POWER, or AC_LINE_UNKNOWN.

    @field BYTE | BatteryFlag | Battery charge status.  Must be one of
	BATTERY_FLAG_HIGH, BATTERY_FLAG_LOW, BATTERY_FLAG_CRITICAL,
	BATTERY_FLAG_CHARGING, BATTERY_FLAG_NO_BATTERY, or BATTERY_FLAG_UNKNOWN

    @field BYTE | BatteryLifePercent | Percentage of full battery charge
    remaining.  Must be in the range 0 to 100, or BATTERY_PERCENTAGE_UNKNOWN.

	@field BYTE | Reserved1 | Must be zero.

    @field DWORD |  BatteryLifeTime | Number of seconds of battery life
    remaining, or BATTERY_LIFE_UNKNOWN if remaining seconds are unknown.

    @field DWORD | BatteryFullLifeTime | Number of seconds of battery life when
    at full charge, or BATTERY_LIFE_UNKNOWN if full lifetime is unknown.

	@field BYTE | Reserved2 | Must be zero.

    @field BYTE | BackupBatteryFlag | Backup battery charge status.  Must be one
    of BATTERY_FLAG_HIGH, BATTERY_FLAG_LOW, BATTERY_FLAG_CRITICAL,
	BATTERY_FLAG_CHARGING, BATTERY_FLAG_NO_BATTERY, or BATTERY_FLAG_UNKNOWN.

    @field BYTE | BackupBatteryLifePercent |  Percentage of full backup battery
    charge remaining.  Must be in the range 0 to 100, or
    BATTERY_PERCENTAGE_UNKNOWN.

	@field BYTE | Reserved3 | Must be zero.

    @field DWORD | BackupBatteryLifeTime | Number of seconds of backup battery
    life remaining, or BATTERY_LIFE_UNKNOWN if remaining seconds are unknown.

    @field DWORD | BackupBatteryFullLifeTime | Number of seconds of backup
    battery life when at full charge, or BATTERY_LIFE_UNKNOWN if full lifetime
    is unknown.

 }

const
   BATTERY_CHEMISTRY_ALKALINE = $01;   
   BATTERY_CHEMISTRY_NICD = $02;   
   BATTERY_CHEMISTRY_NIMH = $03;   
   BATTERY_CHEMISTRY_LION = $04;   
   BATTERY_CHEMISTRY_LIPOLY = $05;   
   BATTERY_CHEMISTRY_ZINCAIR = $06;   
   BATTERY_CHEMISTRY_UNKNOWN = $FF;   
{ Above here is old struct, below are new fields }
{ Reports Reading of battery voltage in millivolts (0..65535 mV) }
{ Reports Instantaneous current drain (mA). 0..32767 for charge, 0 to -32768 for discharge }
{ Reports short term average of device current drain (mA). 0..32767 for charge, 0 to -32768 for discharge }
{ Reports time constant (mS) of integration used in reporting BatteryAverageCurrent }
{ Reports long-term cumulative average DISCHARGE (mAH). Reset by charging or changing the batteries. 0 to 32767 mAH }
{ Reports Battery temp in 0.1 degree C (-3276.8 to 3276.7 degrees C) }
{ Reports Reading of backup battery voltage }
{ See Chemistry defines above }
{ New fields can be added below, but don't change any existing ones }
type

   _SYSTEM_POWER_STATUS_EX2 = record
        ACLineStatus : BYTE;
        BatteryFlag : BYTE;
        BatteryLifePercent : BYTE;
        Reserved1 : BYTE;
        BatteryLifeTime : DWORD;
        BatteryFullLifeTime : DWORD;
        Reserved2 : BYTE;
        BackupBatteryFlag : BYTE;
        BackupBatteryLifePercent : BYTE;
        Reserved3 : BYTE;
        BackupBatteryLifeTime : DWORD;
        BackupBatteryFullLifeTime : DWORD;
        BatteryVoltage : DWORD;
        BatteryCurrent : DWORD;
        BatteryAverageCurrent : DWORD;
        BatteryAverageInterval : DWORD;
        BatterymAHourConsumed : DWORD;
        BatteryTemperature : DWORD;
        BackupBatteryVoltage : DWORD;
        BatteryChemistry : BYTE;
     end;
   SYSTEM_POWER_STATUS_EX2 = _SYSTEM_POWER_STATUS_EX2;
   PSYSTEM_POWER_STATUS_EX2 = ^_SYSTEM_POWER_STATUS_EX2;
   LPSYSTEM_POWER_STATUS_EX2 = ^_SYSTEM_POWER_STATUS_EX2;
{ @CESYSGEN IF COREDLL_BATTERY }

function GetSystemPowerStatusEx(var SystemPowerStatusEx: SYSTEM_POWER_STATUS_EX; fUpdate:BOOL):BOOL;external KernelDLL name 'GetSystemPowerStatusEx';
{ Return value : 0 = fail.  Non-zero indicates length of returned data. }
function GetSystemPowerStatusEx2(var SystemPowerStatusEx2: SYSTEM_POWER_STATUS_EX2; dwLen:DWORD; fUpdate:BOOL):DWORD;external KernelDLL name 'GetSystemPowerStatusEx2';

{$endif read_interface}
