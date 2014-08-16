{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2012 Nikolay Nikolov (nickysn@users.sourceforge.net)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

    As a special exception, the copyright holders of this library give you
    permission to link this library with independent modules to produce an
    executable, regardless of the license terms of these independent modules,and
    to copy and distribute the resulting executable under terms of your choice,
    provided that you also meet, for each linked independent module, the terms
    and conditions of the license of that module. An independent module is a
    module which is not derived from or based on this library. If you modify
    this library, you may extend this exception to your version of the library,
    but you are not obligated to do so. If you do not wish to do so, delete this
    exception statement from your version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{$ifdef VER2_6}
{ constants moved to the windows unit in fpc trunk }
const
  DISP_CHANGE_BADPARAM = -5;
  DISP_CHANGE_BADDUALVIEW = -6;
  DM_POSITION = $00000020;
  DM_NUP = $00000040;
  DM_PANNINGWIDTH = $08000000;
  DM_PANNINGHEIGHT = $10000000;
  DMDFO_DEFAULT = 0;
  DMDFO_STRETCH = 1;
  DMDFO_CENTER  = 2;
{$endif VER2_6}

constructor TWin32ModeSetter.Create;
begin
  SetupModeList;
end;

procedure TWin32ModeSetter.Open(AWidth, AHeight: Integer; AFormat: IPTCFormat);
var
  dm: TDEVMODE;
begin
  FillChar(dm, SizeOf(dm), 0);
  dm.dmSize := SizeOf(dm);
  dm.dmPelsWidth := AWidth;
  dm.dmPelsHeight := AHeight;
  dm.dmBitsPerPel := AFormat.Bits;
  dm.dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;

  DispChangeCheck(ChangeDisplaySettings(@dm, CDS_FULLSCREEN));

  FillChar(FChosenMode, SizeOf(FChosenMode), 0);
  FChosenMode.dmSize := SizeOf(FChosenMode);
  FChosenMode.dmDriverExtra := 0;
  if not EnumDisplaySettings(nil, ENUM_CURRENT_SETTINGS, @FChosenMode) then
    raise TPTCError.Create('EnumDisplaySettings(ENUM_CURRENT_SETTINGS) failed after mode set');

  FInMode := True;
  FOpen := True;
end;

procedure TWin32ModeSetter.Close;
begin
  FOpen := False;
  if not FInMode then
    exit;

  DispChangeCheck(ChangeDisplaySettings(nil, 0));
  FInMode := False;
end;

procedure TWin32ModeSetter.Save;
begin
  LOG('saving desktop');
end;

procedure TWin32ModeSetter.Restore;
begin
  LOG('restoring desktop');
end;

procedure TWin32ModeSetter.Enter;
begin
  LOG('entering mode');
  if not FInMode then
  begin
    DispChangeCheck(ChangeDisplaySettings(@FChosenMode, CDS_FULLSCREEN));
    FInMode := True;
  end;
end;

procedure TWin32ModeSetter.Leave;
begin
  LOG('leaving mode');
  if FInMode then
  begin
    DispChangeCheck(ChangeDisplaySettings(nil, 0));
    FInMode := False;
  end;
end;

procedure TWin32ModeSetter.SetupModeList;
var
  dm: TDEVMODE;
  I: Integer;
  ModeExists: Boolean;
begin
  LOG('getting list of display modes');
  SetLength(FModes, 0);
  I := 0;
  repeat
    FillChar(dm, SizeOf(dm), 0);
    dm.dmSize := SizeOf(dm);
    dm.dmDriverExtra := 0;
    ModeExists := EnumDisplaySettings(nil, I, @dm);
    if ModeExists then
    begin
      LogDevMode(dm);
      LOG(IntToStr(dm.dmPelsWidth) + 'x' + IntToStr(dm.dmPelsHeight) + 'x' + IntToStr(dm.dmBitsPerPel) + ' ' + IntToStr(dm.dmDisplayFrequency) + ' Hz');
      {todo: add to FModes list...}
      Inc(I);
    end;
  until not ModeExists;
  LOG('done getting the list of modes');
end;

procedure TWin32ModeSetter.DispChangeCheck(ADispChangeResult: LONG);

  function DispChangeResult2String(ADispChangeResult: LONG): string;
  begin
    case ADispChangeResult of
      DISP_CHANGE_SUCCESSFUL:  Result := 'DISP_CHANGE_SUCCESSFUL';
      DISP_CHANGE_BADDUALVIEW: Result := 'DISP_CHANGE_BADDUALVIEW';
      DISP_CHANGE_BADFLAGS:    Result := 'DISP_CHANGE_BADFLAGS';
      DISP_CHANGE_BADMODE:     Result := 'DISP_CHANGE_BADMODE';
      DISP_CHANGE_BADPARAM:    Result := 'DISP_CHANGE_BADPARAM';
      DISP_CHANGE_FAILED:      Result := 'DISP_CHANGE_FAILED';
      DISP_CHANGE_NOTUPDATED:  Result := 'DISP_CHANGE_NOTUPDATED';
      DISP_CHANGE_RESTART:     Result := 'DISP_CHANGE_RESTART';
      else
        Result := 'Unknown';
    end;
    Result := IntToStr(ADispChangeResult) + ' (' + Result + ')';
  end;

begin
  if ADispChangeResult <> DISP_CHANGE_SUCCESSFUL then
    raise TPTCError.Create('Error setting display mode; ChangeDisplaySettings returned ' + DispChangeResult2String(ADispChangeResult));
end;

procedure TWin32ModeSetter.LogDevMode(const ADevMode: TDEVMODE);

  function Fields2String(dmFields: DWORD): string;
  begin
    Result := '';
    if (dmFields and DM_ORIENTATION) <> 0 then
      Result := Result + 'DM_ORIENTATION + ';
    if (dmFields and DM_PAPERSIZE) <> 0 then
      Result := Result + 'DM_PAPERSIZE + ';
    if (dmFields and DM_PAPERLENGTH) <> 0 then
      Result := Result + 'DM_PAPERLENGTH + ';
    if (dmFields and DM_PAPERWIDTH) <> 0 then
      Result := Result + 'DM_PAPERWIDTH + ';
    if (dmFields and DM_SCALE) <> 0 then
      Result := Result + 'DM_SCALE + ';
    if (dmFields and DM_COPIES) <> 0 then
      Result := Result + 'DM_COPIES + ';
    if (dmFields and DM_DEFAULTSOURCE) <> 0 then
      Result := Result + 'DM_DEFAULTSOURCE + ';
    if (dmFields and DM_PRINTQUALITY) <> 0 then
      Result := Result + 'DM_PRINTQUALITY + ';
    if (dmFields and DM_POSITION) <> 0 then
      Result := Result + 'DM_POSITION + ';
    if (dmFields and DM_DISPLAYORIENTATION) <> 0 then
      Result := Result + 'DM_DISPLAYORIENTATION + ';
    if (dmFields and DM_DISPLAYFIXEDOUTPUT) <> 0 then
      Result := Result + 'DM_DISPLAYFIXEDOUTPUT + ';
    if (dmFields and DM_COLOR) <> 0 then
      Result := Result + 'DM_COLOR + ';
    if (dmFields and DM_DUPLEX) <> 0 then
      Result := Result + 'DM_DUPLEX + ';
    if (dmFields and DM_YRESOLUTION) <> 0 then
      Result := Result + 'DM_YRESOLUTION + ';
    if (dmFields and DM_TTOPTION) <> 0 then
      Result := Result + 'DM_TTOPTION + ';
    if (dmFields and DM_COLLATE) <> 0 then
      Result := Result + 'DM_COLLATE + ';
    if (dmFields and DM_FORMNAME) <> 0 then
      Result := Result + 'DM_FORMNAME + ';
    if (dmFields and DM_LOGPIXELS) <> 0 then
      Result := Result + 'DM_LOGPIXELS + ';
    if (dmFields and DM_BITSPERPEL) <> 0 then
      Result := Result + 'DM_BITSPERPEL + ';
    if (dmFields and DM_PELSWIDTH) <> 0 then
      Result := Result + 'DM_PELSWIDTH + ';
    if (dmFields and DM_PELSHEIGHT) <> 0 then
      Result := Result + 'DM_PELSHEIGHT + ';
    if (dmFields and DM_DISPLAYFLAGS) <> 0 then
      Result := Result + 'DM_DISPLAYFLAGS + ';
    if (dmFields and DM_NUP) <> 0 then
      Result := Result + 'DM_NUP + ';
    if (dmFields and DM_DISPLAYFREQUENCY) <> 0 then
      Result := Result + 'DM_DISPLAYFREQUENCY + ';
    if (dmFields and DM_ICMMETHOD) <> 0 then
      Result := Result + 'DM_ICMMETHOD + ';
    if (dmFields and DM_ICMINTENT) <> 0 then
      Result := Result + 'DM_ICMINTENT + ';
    if (dmFields and DM_MEDIATYPE) <> 0 then
      Result := Result + 'DM_MEDIATYPE + ';
    if (dmFields and DM_DITHERTYPE) <> 0 then
      Result := Result + 'DM_DITHERTYPE + ';
    if (dmFields and DM_PANNINGWIDTH) <> 0 then
      Result := Result + 'DM_PANNINGWIDTH + ';
    if (dmFields and DM_PANNINGHEIGHT) <> 0 then
      Result := Result + 'DM_PANNINGHEIGHT + ';

    if Length(Result) > 0 then
      Result := Copy(Result, 1, Length(Result) - 3);

    Result := IntToStr(dmFields) + ' (' + Result + ')';
  end;

  function DisplayOrientation2String(dmDisplayOrientation: DWORD): string;
  begin
    case dmDisplayOrientation of
      DMDO_DEFAULT: Result := 'DMDO_DEFAULT';
      DMDO_90:      Result := 'DMDO_90';
      DMDO_180:     Result := 'DMDO_180';
      DMDO_270:     Result := 'DMDO_270';
      else
        Result := 'Unknown';
    end;
    Result := IntToStr(dmDisplayOrientation) + ' (' + Result + ')';
  end;

  function DisplayFixedOutput2String(dmDisplayFixedOutput: DWORD): string;
  begin
    case dmDisplayFixedOutput of
      DMDFO_DEFAULT: Result := 'DMDFO_DEFAULT';
      DMDFO_CENTER:  Result := 'DMDFO_CENTER';
      DMDFO_STRETCH: Result := 'DMDFO_STRETCH';
      else
        Result := 'Unknown';
    end;
    Result := IntToStr(dmDisplayFixedOutput) + ' (' + Result + ')';
  end;

  function DisplayFlags2String(dmDisplayFlags: DWORD): string;
  begin
    Result := '';
    if (dmDisplayFlags and DM_GRAYSCALE) <> 0 then
      Result := Result + 'DM_GRAYSCALE + ';
    if (dmDisplayFlags and DM_INTERLACED) <> 0 then
      Result := Result + 'DM_INTERLACED + ';

    if Length(Result) > 0 then
      Result := Copy(Result, 1, Length(Result) - 3);

    Result := IntToStr(dmDisplayFlags) + ' (' + Result + ')';
  end;

begin
  LOG('dmFields', Fields2String(ADevMode.dmFields));
  if (ADevMode.dmFields and DM_DISPLAYORIENTATION) <> 0 then
    LOG('dmDisplayOrientation', DisplayOrientation2String(ADevMode.dmDisplayOrientation));
  if (ADevMode.dmFields and DM_DISPLAYFIXEDOUTPUT) <> 0 then
    LOG('dmDisplayFixedOutput', DisplayFixedOutput2String(ADevMode.dmDisplayFixedOutput));
  if (ADevMode.dmFields and DM_BITSPERPEL) <> 0 then
    LOG('dmBitsPerPel        ', ADevMode.dmBitsPerPel);
  if (ADevMode.dmFields and DM_PELSWIDTH) <> 0 then
    LOG('dmPelsWidth         ', ADevMode.dmPelsWidth);
  if (ADevMode.dmFields and DM_PELSHEIGHT) <> 0 then
    LOG('dmPelsHeight        ', ADevMode.dmPelsHeight);
  if (ADevMode.dmFields and DM_DISPLAYFLAGS) <> 0 then
    LOG('dmDisplayFlags      ', DisplayFlags2String(ADevMode.dmDisplayFlags));
  if (ADevMode.dmFields and DM_DISPLAYFREQUENCY) <> 0 then
    LOG('dmDisplayFrequency  ', IntToStr(ADevMode.dmDisplayFrequency) + ' Hz');
end;
