{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2023 the Free Pascal development team.

    Delphi compatibility unit to provide a stopwatch.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
// Todo: better resolution for non-windows, non-linux:
// macos should have mach_absolute_time somewhere.
// FreeBSD should have clock_gettime routines, but they seem not to be exposed in FPC units?
unit system.diagnostics;

{$mode objfpc}
{$modeswitch advancedrecords}

interface

uses System.TimeSpan;

const
  StopWatchResolution = 10*1000*1000; // 0.1 microsecond
  TicksPerMillisecond = 10*1000;
  TicksPerSecond      = StopWatchResolution;

type

  { TStopwatch }

  TStopwatch = record
  private
    class var _Frequency: Int64;
    class var _IsHighResolution: Boolean;
    class var _TickFrequency: Double;
    Class procedure _Init; static;
  private
    FElapsed: Int64;
    FRunning: Boolean;
    FStartTimeStamp: Int64;
    function GetElapsedTimespanTicks: Int64; inline;
    function GetElapsed: TTimeSpan;
    function GetElapsedMilliseconds: Int64;
    function GetElapsedTicks: Int64;
  public
    class function Create: TStopwatch; static;
    class function GetTimeStamp: Int64; static;
    class function StartNew: TStopwatch; static;
    class property Frequency: Int64 read _Frequency;
    class property IsHighResolution: Boolean read _IsHighResolution;
  public  
    procedure Reset;
    procedure Start;
    procedure Stop;
    property Elapsed: TTimeSpan read GetElapsed;
    property ElapsedMilliseconds: Int64 read GetElapsedMilliseconds;
    property ElapsedTicks: Int64 read GetElapsedTicks;
    property IsRunning: Boolean read FRunning;
  end;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  {$IFDEF WINDOWS}
  Winapi.Windows,
  {$ELSE}
  {$IFDEF LINUX}
  UnixApi.Types,
  LinuxApi,
  {$ENDIF LINUX}
  {$ENDIF WINDOWS}
  System.SysUtils;

{$ELSE FPC_DOTTEDUNITS}

  {$IFDEF WINDOWS}
  Windows,
  {$ELSE}
  {$IFDEF LINUX}
  UnixType,
  Linux,
  {$ENDIF LINUX}
  {$ENDIF WINDOWS}
  SysUtils;

{$ENDIF FPC_DOTTEDUNITS}

{ TStopwatch }

function TStopwatch.GetElapsedTimespanTicks: Int64;

begin
  Result:=ElapsedTicks;
  if _IsHighResolution then
    Result:=Trunc(Result*_TickFrequency);
end;


function TStopwatch.GetElapsed: TTimeSpan;

begin
  Result:=TTimeSpan.Create(GetElapsedTimeSpanTicks);
end;


function TStopwatch.GetElapsedMilliseconds: Int64;

begin
  Result:=GetElapsedTimeSpanTicks div TicksPerMillisecond;
end;


function TStopwatch.GetElapsedTicks: Int64;

begin
  Result:=FElapsed;
  if Not FRunning then
    exit;
  Result:=Result+GetTimeStamp-FStartTimeStamp;
end;


class function TStopwatch.Create: TStopwatch;

begin
  Result.Reset;
end;


class function TStopwatch.StartNew: TStopwatch;

begin
  Result.Reset;
  Result.Start;
end;


procedure TStopwatch.Reset;

begin
  FElapsed:=0;
  FRunning:=False;
  FStartTimeStamp:=0;
end;


procedure TStopwatch.Start;

begin
  if FRunning then
    exit;
  FRunning:=True;
  FStartTimeStamp:=GetTimeStamp;
end;


procedure TStopwatch.Stop;

begin
  if Not FRunning then
    exit;
  FRunning:=False;
  Inc(FElapsed,(GetTimeStamp-FStartTimeStamp));
end;


{$IFDEF LINUX}
class function TStopwatch.GetTimeStamp: Int64;

var
  res: timespec;

begin
  clock_gettime(CLOCK_MONOTONIC, @res);
  Result:=((StopWatchResolution*res.tv_sec)+res.tv_nsec) div 100;
end;


class procedure TStopwatch._Init;

begin
  _IsHighResolution:=True;
  _Frequency:=StopWatchResolution;
  _TickFrequency:=1;
end;

{$ELSE UNIX}

{$IFDEF WINDOWS}
class function TStopwatch.GetTimeStamp: Int64;

begin
  if _IsHighResolution then
    QueryPerformanceCounter(Result)
  else
    Result:=GetTickCount64*TicksPerMillisecond;
end;


class procedure TStopWatch._Init;

begin
  _IsHighResolution:=QueryPerformanceFrequency(_Frequency);
  if _IsHighResolution then
    TStopWatch._TickFrequency:=StopWatchResolution/_Frequency
  else
    begin
    _TickFrequency:=1;
    _Frequency:=TicksPerSecond;
    end;
end;

{$ELSE WINDOWS}

class procedure TStopWatch._Init;

begin
  _IsHighResolution:=False;
  _TickFrequency:=1;
  _Frequency:=TicksPerSecond;
end;


class function TStopwatch.GetTimeStamp: Int64;
begin
  Result:=GetTickCount*TicksPerMillisecond;
end;

{$ENDIF WINDOWS}
{$ENDIF UNIX}

initialization
  TStopWatch._Init;
end.
