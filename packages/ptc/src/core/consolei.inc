{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C++ version by Glenn Fiedler (ptc@gaffer.org)

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

const
 {$IFDEF GO32V2}
  ConsoleTypesNumber = 4;
 {$ENDIF GO32V2}
 {$IF defined(Win32) OR defined(Win64)}
  ConsoleTypesNumber = 2;
 {$ENDIF defined(Win32) OR defined(Win64)}
 {$IFDEF WinCE}
  ConsoleTypesNumber = 2;
 {$ENDIF WinCE}
 {$IFDEF UNIX}
  ConsoleTypesNumber = 1;
 {$ENDIF UNIX}
  ConsoleTypes: array [0..ConsoleTypesNumber - 1] of
    record
      ConsoleClass: class of TPTCBaseConsole;
      Names: array [1..2] of string;
    end =
  (
  {$IFDEF GO32V2}
   (ConsoleClass: TVESAConsole;      Names: ('VESA', '')),
   (ConsoleClass: TVGAConsole;       Names: ('VGA', 'Fakemode')),
   (ConsoleClass: TCGAConsole;       Names: ('CGA', '')),
   (ConsoleClass: TTEXTFX2Console;   Names: ('TEXTFX2', 'Text'))
  {$ENDIF GO32V2}

  {$IF defined(Win32) OR defined(Win64)}
   (ConsoleClass: TDirectXConsole;   Names: ('DirectX', '')),
   (ConsoleClass: TGDIConsole;       Names: ('GDI', ''))
  {$ENDIF defined(Win32) OR defined(Win64)}

  {$IFDEF WinCE}
   (ConsoleClass: TWinCEGAPIConsole; Names: ('GAPI', '')),
   (ConsoleClass: TWinCEGDIConsole;  Names: ('GDI', ''))
  {$ENDIF WinCE}

  {$IFDEF UNIX}
   (ConsoleClass: TX11Console;       Names: ('X11', ''))
  {$ENDIF UNIX}
  );

constructor TPTCConsole.Create;
var
  I: Integer;
  {$IFDEF UNIX}
  s: AnsiString;
  {$ENDIF UNIX}
begin
  inherited Create;
  FConsole := nil;
  FHackyOptionConsoleFlag := False;
  FillChar(FModes, SizeOf(FModes), 0);
  for I := Low(FModes) to High(FModes) do
    FModes[I] := TPTCMode.Create;

  {$IFDEF UNIX}
    Configure('/usr/share/ptcpas/ptcpas.conf');
    s := fpgetenv('HOME');
    if s = '' then
      s := '/';
    if s[Length(s)] <> '/' then
      s := s + '/';
    s := s + '.ptcpas.conf';
    Configure(s);
  {$ENDIF UNIX}

  {$IFDEF Win32}
    Configure('ptcpas.cfg');
  {$ENDIF Win32}

  {$IFDEF GO32V2}
    Configure('ptcpas.cfg');
  {$ENDIF GO32V2}

  {$IFDEF WinCE}
  {todo: configure WinCE}
  {$ENDIF WinCE}
end;

destructor TPTCConsole.Destroy;
var
  I: Integer;
begin
  close;
  FConsole.Free;
  for I := Low(FModes) to High(FModes) do
    FModes[I].Free;
  inherited Destroy;
end;

procedure TPTCConsole.Configure(const AFile: string);
var
  F: TextFile;
  S: string;
begin
  AssignFile(F, AFile);
  {$I-}
  Reset(F);
  {$I+}
  if IOResult <> 0 then
    exit;
  while not EoF(F) do
  begin
    {$I-}
    Readln(F, S);
    {$I+}
    if IOResult <> 0 then
      Break;
    Option(S);
  end;
  CloseFile(F);
end;

procedure TPTCConsole.AddOptionToOptionsQueue(const AOption: string);
begin
  SetLength(FOptionsQueue, Length(FOptionsQueue) + 1);
  FOptionsQueue[High(FOptionsQueue)] := AOption;
end;

procedure TPTCConsole.ExecuteOptionsFromOptionsQueue;
var
  I: Integer;
begin
  for I := Low(FOptionsQueue) to High(FOptionsQueue) do
    FConsole.Option(FOptionsQueue[I]);
end;

procedure TPTCConsole.ClearOptionsQueue;
begin
  SetLength(FOptionsQueue, 0);
end;

function TPTCConsole.Option(const AOption: String): Boolean;
begin
  if AOption = 'enable logging' then
  begin
    LOG_enabled := True;
    Result := True;
    exit;
  end;
  if AOption = 'disable logging' then
  begin
    LOG_enabled := False;
    Result := True;
    exit;
  end;

  if Assigned(FConsole) then
    Result := FConsole.Option(AOption)
  else
  begin
    FConsole := ConsoleCreate(AOption);
    if Assigned(FConsole) then
    begin
      FHackyOptionConsoleFlag := True;
      ExecuteOptionsFromOptionsQueue;
{      ClearOptionsQueue;}
      Result := True;
    end
    else
    begin
      { TODO: check if the option is supported by at least one console... }
      if {OptionSupported}True then
      begin
        AddOptionToOptionsQueue(AOption);
        Result := True;
      end
      else
        Result := False;
    end;
  end;
end;

function TPTCConsole.Modes: PPTCMode;
var
  _console: TPTCBaseConsole;
  index, mode: Integer;
  local: Integer;
  _modes: PPTCMode;
  tmp: TPTCMode;
begin
  if Assigned(FConsole) then
    Result := FConsole.Modes
  else
  begin
    _console := nil;
    index := -1;
    mode := 0;
    try
      repeat
        Inc(index);
        try
          _console := ConsoleCreate(index);
        except
          on TPTCError do begin
            FreeAndNil(_console);
            Continue;
          end;
        end;
        if _console = nil then
          Break;
        _modes := _console.modes;
        local := 0;
        while _modes[local].valid do
        begin
          FModes[mode].Assign(_modes[local]);
          Inc(local);
          Inc(mode);
        end;
        FreeAndNil(_console);
      until False;
    finally
      _console.Free;
    end;
    { todo: strip duplicate modes from list? }
    tmp := TPTCMode.Create;
    try
      FModes[mode].Assign(tmp);
    finally
      tmp.Free;
    end;
    Result := FModes;
  end;
end;

procedure TPTCConsole.Open(const ATitle: string; APages: Integer);
var
  composite, tmp: TPTCError;
  index: Integer;
  success: Boolean;
begin
  if Assigned(FConsole) then
  begin
    try
      FConsole.open(ATitle, APages);
      exit;
    except
      on error: TPTCError do begin
        FreeAndNil(FConsole);
        if FHackyOptionConsoleFlag then
        begin
          FHackyOptionConsoleFlag := False;
          raise TPTCError.Create('could not open console', error);
        end;
      end;
    end;
  end;
  index := -1;
  composite := TPTCError.Create;
  success := False;
  try
    repeat
      Inc(index);
      try
        FConsole := ConsoleCreate(index);
        if FConsole = nil then
          break;
        ExecuteOptionsFromOptionsQueue;
        FConsole.Open(ATitle, APages);
{        ClearOptionsQueue;}
        success := True;
        exit;
      except
        on error: TPTCError do begin
          tmp := TPTCError.Create(error.message, composite);
          try
            composite.Assign(tmp);
          finally
            tmp.Free;
          end;
          FreeAndNil(FConsole);
          continue;
        end;
      end;
    until False;
    FConsole := nil;
    raise TPTCError.Create(composite);
  finally
    composite.Free;
    if not success then
      FreeAndNil(FConsole);
  end;
end;

procedure TPTCConsole.Open(const ATitle: string; const AFormat: TPTCFormat;
                           APages: Integer);
var
  composite, tmp: TPTCError;
  index: Integer;
  success: Boolean;
begin
  if Assigned(FConsole) then
  begin
    try
      FConsole.open(ATitle, AFormat, APages);
      exit;
    except
      on error: TPTCError do begin
        FreeAndNil(FConsole);
        if FHackyOptionConsoleFlag then
        begin
          FHackyOptionConsoleFlag := False;
          raise TPTCError.Create('could not open console', error);
        end;
      end;
    end;
  end;
  index := -1;
  composite := TPTCError.Create;
  success := False;
  try
    repeat
      Inc(index);
      try
        FConsole := ConsoleCreate(index);
        if FConsole = nil then
          break;
        ExecuteOptionsFromOptionsQueue;
        FConsole.open(ATitle, AFormat, APages);
{        ClearOptionsQueue;}
        success := True;
        exit;
      except
        on error: TPTCError do begin
          tmp := TPTCError.Create(error.message, composite);
          try
            composite.Assign(tmp);
          finally
            tmp.Free;
          end;
          FreeAndNil(FConsole);
          Continue;
        end;
      end;
    until False;
    FConsole := nil;
    raise TPTCError.Create(composite);
  finally
    composite.Free;
    if not success then
      FreeAndNil(FConsole);
  end;
end;

procedure TPTCConsole.Open(const ATitle: string; AWidth, AHeight: Integer;
                           const AFormat: TPTCFormat; APages: Integer);
var
  composite, tmp: TPTCError;
  index: Integer;
  success: Boolean;
begin
  if Assigned(FConsole) then
  begin
    try
      FConsole.Open(ATitle, AWidth, AHeight, AFormat, APages);
      exit;
    except
      on error: TPTCError do begin
        FreeAndNil(FConsole);
        if FHackyOptionConsoleFlag then
        begin
          FHackyOptionConsoleFlag := False;
          raise TPTCError.Create('could not open console', error);
        end;
      end;
    end;
  end;
  index := -1;
  composite := TPTCError.Create;
  success := False;
  try
    repeat
      Inc(index);
      try
        FConsole := ConsoleCreate(index);
        if FConsole = nil then
          Break;
        ExecuteOptionsFromOptionsQueue;
        FConsole.Open(ATitle, AWidth, AHeight, AFormat, APages);
{        ClearOptionsQueue;}
        success := True;
        exit;
      except
        on error: TPTCError do begin
          tmp := TPTCError.Create(error.message, composite);
          try
            composite.Assign(tmp);
          finally
            tmp.Free;
          end;
          FreeAndNil(FConsole);
          Continue;
        end;
      end;
    until False;
    FConsole := nil;
    raise TPTCError.Create(composite);
  finally
    composite.Free;
    if not success then
      FreeAndNil(FConsole);
  end;
end;

procedure TPTCConsole.Open(const ATitle: string; const AMode: TPTCMode;
                           APages: Integer);
var
  composite, tmp: TPTCError;
  index: Integer;
  success: Boolean;
begin
  if Assigned(FConsole) then
  begin
    try
      FConsole.Open(ATitle, AMode, APages);
      exit;
    except
      on error: TPTCError do begin
        FreeAndNil(FConsole);
        if FHackyOptionConsoleFlag then
        begin
          FHackyOptionConsoleFlag := False;
          raise TPTCError.Create('could not open console', error);
        end;
      end;
    end;
  end;
  index := -1;
  composite := TPTCError.Create;
  success := False;
  try
    repeat
      Inc(index);
      try
        FConsole := ConsoleCreate(index);
        if FConsole = nil then
          Break;
        ExecuteOptionsFromOptionsQueue;
        FConsole.Open(ATitle, AMode, APages);
{        ClearOptionsQueue;}
        success := True;
        exit;
      except
        on error: TPTCError do begin
          tmp := TPTCError.Create(error.message, composite);
          try
            composite.Assign(tmp);
          finally
            tmp.Free;
          end;
          FreeAndNil(FConsole);
          Continue;
        end;
      end;
    until False;
    FConsole := nil;
    raise TPTCError.Create(composite);
  finally
    composite.Free;
    if not success then
      FreeAndNil(FConsole);
  end;
end;

procedure TPTCConsole.Close;
begin
  if Assigned(FConsole) then
    FConsole.Close;
  FHackyOptionConsoleFlag := False;
end;

procedure TPTCConsole.Flush;
begin
  Check;
  FConsole.Flush;
end;

procedure TPTCConsole.Finish;
begin
  Check;
  FConsole.Finish;
end;

procedure TPTCConsole.Update;
begin
  Check;
  FConsole.Update;
end;

procedure TPTCConsole.Update(const AArea: TPTCArea);
begin
  Check;
  FConsole.Update(AArea);
end;

procedure TPTCConsole.Copy(ASurface: TPTCBaseSurface);
begin
  Check;
  FConsole.Copy(ASurface);
end;

procedure TPTCConsole.Copy(ASurface: TPTCBaseSurface;
                           const ASource, ADestination: TPTCArea);
begin
  Check;
  FConsole.Copy(ASurface, ASource, ADestination);
end;

function TPTCConsole.Lock: Pointer;
begin
  Check;
  Result := FConsole.Lock;
end;

procedure TPTCConsole.Unlock;
begin
  Check;
  FConsole.Unlock;
end;

procedure TPTCConsole.Load(const APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           const AFormat: TPTCFormat;
                           const APalette: TPTCPalette);
begin
  Check;
  FConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette);
end;

procedure TPTCConsole.Load(const APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           const AFormat: TPTCFormat;
                           const APalette: TPTCPalette;
                           const ASource, ADestination: TPTCArea);
begin
  Check;
  FConsole.Load(APixels, AWidth, AHeight, APitch, AFormat, APalette,
                ASource, ADestination);
end;

procedure TPTCConsole.Save(Apixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           const AFormat: TPTCFormat;
                           const APalette: TPTCPalette);
begin
  Check;
  FConsole.Save(APixels, AWidth, AHeight, APitch, AFormat, APalette);
end;

procedure TPTCConsole.Save(APixels: Pointer;
                           AWidth, AHeight, APitch: Integer;
                           const AFormat: TPTCFormat;
                           const APalette: TPTCPalette;
                           const ASource, ADestination: TPTCArea);
begin
  Check;
  FConsole.Save(APixels, AWidth, AHeight, APitch, AFormat, APalette,
                ASource, ADestination);
end;

procedure TPTCConsole.Clear;
begin
  Check;
  FConsole.clear;
end;

procedure TPTCConsole.Clear(const AColor: TPTCColor);
begin
  Check;
  FConsole.clear(AColor);
end;

procedure TPTCConsole.Clear(const AColor: TPTCColor;
                           const AArea: TPTCArea);
begin
  Check;
  FConsole.clear(AColor, AArea);
end;

procedure TPTCConsole.Palette(const APalette: TPTCPalette);
begin
  Check;
  FConsole.Palette(APalette);
end;

function TPTCConsole.Palette: TPTCPalette;
begin
  Check;
  Result := FConsole.Palette;
end;

procedure TPTCConsole.Clip(const AArea: TPTCArea);
begin
  Check;
  FConsole.Clip(AArea);
end;

function TPTCConsole.GetWidth: Integer;
begin
  Check;
  Result := FConsole.GetWidth;
end;

function TPTCConsole.GetHeight: Integer;
begin
  Check;
  Result := FConsole.GetHeight;
end;

function TPTCConsole.GetPitch: Integer;
begin
  Check;
  Result := FConsole.GetPitch;
end;

function TPTCConsole.GetPages: Integer;
begin
  Check;
  Result := FConsole.GetPages;
end;

function TPTCConsole.GetArea: TPTCArea;
begin
  Check;
  Result := FConsole.GetArea;
end;

function TPTCConsole.Clip: TPTCArea;
begin
  Check;
  Result := FConsole.Clip;
end;

function TPTCConsole.GetFormat: TPTCFormat;
begin
  Check;
  Result := FConsole.GetFormat;
end;

function TPTCConsole.GetName: string;
begin
  Result := '';
  if Assigned(FConsole) then
    Result := FConsole.GetName
  else
{$IFDEF GO32V2}
    Result := 'DOS';
{$ENDIF GO32V2}
{$IFDEF WIN32}
    Result := 'Win32';
{$ENDIF WIN32}
{$IFDEF WIN64}
    Result := 'Win64';
{$ENDIF WIN64}
{$IFDEF LINUX}
    Result := 'Linux';
{$ENDIF LINUX}
end;

function TPTCConsole.GetTitle: string;
begin
  Check;
  Result := FConsole.GetTitle;
end;

function TPTCConsole.GetInformation: string;
begin
  Check;
  Result := FConsole.GetInformation;
end;

function TPTCConsole.NextEvent(var AEvent: TPTCEvent; AWait: Boolean; const AEventMask: TPTCEventMask): Boolean;
begin
  Check;
  Result := FConsole.NextEvent(AEvent, AWait, AEventMask);
end;

function TPTCConsole.PeekEvent(AWait: Boolean; const AEventMask: TPTCEventMask): TPTCEvent;
begin
  Check;
  Result := FConsole.PeekEvent(AWait, AEventMask);
end;

function TPTCConsole.ConsoleCreate(AIndex: Integer): TPTCBaseConsole;
begin
  Result := nil;
  if (AIndex >= Low(ConsoleTypes)) and (AIndex <= High(ConsoleTypes)) then
    Result := ConsoleTypes[AIndex].ConsoleClass.Create;

  if Result <> nil then
    Result.KeyReleaseEnabled := KeyReleaseEnabled;
end;

function TPTCConsole.ConsoleCreate(const AName: string): TPTCBaseConsole;
var
  I, J: Integer;
begin
  Result := nil;

  if AName = '' then
    exit;

  for I := Low(ConsoleTypes) to High(ConsoleTypes) do
    for J := Low(ConsoleTypes[I].Names) to High(ConsoleTypes[I].Names) do
      if AName = ConsoleTypes[I].Names[J] then
      begin
        Result := ConsoleTypes[I].ConsoleClass.Create;

        if Result <> nil then
        begin
          Result.KeyReleaseEnabled := KeyReleaseEnabled;
          exit;
        end;
      end;
end;

procedure TPTCConsole.Check;
begin
  if FConsole = nil then
    raise TPTCError.Create('console is not open (core)');
end;
