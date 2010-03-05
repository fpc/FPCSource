program navapitest;

{$APPTYPE CONSOLE}
{$ifdef fpc}{$mode delphi}{$endif}

uses
  SysUtils,
  NvAPI;

procedure TestGraphicAPI;
var
  info  : NV_DISPLAY_DRIVER_VERSION;
  res   : NvAPI_Status;
begin
  writeln('Graphic Driver API: ' );
  FillChar(info, sizeof(info), 0);
  info.version:=NV_DISPLAY_DRIVER_VERSION_VER;
  res:=NvAPI_GetDisplayDriverVersion(0, @info);
  if res= NVAPI_OK then begin
    writeln('DriverVer: ', info.drvVersion div 100, '.', info.drvVersion mod 100);
    writeln('Branch:    ', info.szBuildBranchString);
    writeln('Adpater:   ', info.szAdapterString);
  end else
    writeln('Not available or Failed (err ', Integer(res),')');
  writeln;
end;


procedure TestGPUAPI;
var
  phys  : TNvPhysicalGpuHandleArray;
  log   : TNvLogicalGpuHandleArray;
  cnt   : LongWord;
  i     : Integer;
  name  : NvAPI_ShortString;
  thermal : TNvGPUThermalSettings;
  res   : NvAPI_Status;
begin
  writeln('GPU API: ' );
  if NvAPI_EnumPhysicalGPUs(phys, cnt) = NVAPI_OK then begin
    writeln('Physical GPUs ', cnt);
    for i:=0 to cnt - 1 do
      if NvAPI_GPU_GetFullName(phys[i], name) = NVAPI_OK then begin
        write('  ', name,' ');
        FillChar(thermal, sizeof(thermal), 0);
        thermal.version:=NV_GPU_THERMAL_SETTINGS_VER;
        res:=NvAPI_GPU_GetThermalSettings(phys[i],0, @thermal);
        if res= NVAPI_OK then
          write('temp: ', thermal.sensor[0].currentTemp, ' C');
        writeln;
      end;
  end;

  if NvAPI_EnumLogicalGPUs(log, cnt) = NVAPI_OK then
    writeln('Logical GPUs ', cnt);

  writeln;
end;

procedure TestDisplayAPI;
var
  i     : Integer;
  hnd   : NvDisplayHandle;
  name  : NvAPI_ShortString;
begin
  writeln('Display APIs: ');
  i:=0;
  hnd:=0;
  while NvAPI_EnumNVidiaDisplayHandle(i, hnd) = NVAPI_OK do begin
    if NvAPI_GetAssociatedNVidiaDisplayName(hnd, name) = NVAPI_OK then
      writeln('Display: ', name);
    inc(i);
  end;
  writeln;
end;

procedure TestStereoscopicAPI;
var
  res : NvAPI_Status;
  b   : NvU8;
begin
  writeln('Stereoscopic API check');
  res:=NvAPI_Stereo_IsEnabled(b);
  if res = NVAPI_OK then begin
    Writeln('Stereo is available');
    if b = 0 then
      writeln('  disabled')
    else
      writeln('  enabled');
  end else
    Writeln('Stereo is unavailable');
  writeln;
end;


procedure TestSystemAPI;
var
  info  : NV_CHIPSET_INFO_v1;
  res   : NvAPI_Status;
begin
  writeln('System APIs: ');
  FillChar(info, sizeof(info), 0);
  info.version:=NV_CHIPSET_INFO_VER_1;
  res:=NvAPI_SYS_GetChipSetInfo ( info );
  if res = NVAPI_OK then begin
    writelN('Vendor:    ', info.szVendorName);
    writeln('Chipset:   ', info.szChipsetName);
    writelN('Vendor ID: ', IntToHex(info.vendorId, 4));
    writelN('Device ID: ', IntToHex(info.deviceId, 4));
  end;
  writeln;
end;

var
  ver  : NvAPI_ShortString;
  res  : NvAPI_Status;

begin
  res:=NvAPI_Initialize;
  if res<>NVAPI_OK then begin
    writeln('unable to initialize NvAPI');
    Exit;
  end;

  writeln('NvAPI test');
  NvAPI_GetInterfaceVersionString(ver);
  writeln('Version: ', ver);
  writeln;

  TestSystemAPI;
  TestGraphicAPI;
  TestDisplayAPI;
  TestGPUAPI;
  TestStereoscopicAPI;
end.
