{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2012 by Sven Barth
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$warning TODO}

function GetExceptionMask: TFPUExceptionMask;
begin
  Result := [];
end;

function SetExceptionMask(const Mask: TFPUExceptionMask): TFPUExceptionMask;
begin
  { TODO }
end;

function GetRoundMode: TFPURoundingMode;
begin
  Result:=rmNearest;
end;

function SetRoundMode(const RoundMode: TFPURoundingMode): TFPURoundingMode;
begin
  Result:=rmNearest;
end;

function GetPrecisionMode: TFPUPrecisionMode;
begin
  result:=pmDouble;
end;

function SetPrecisionMode(const Precision: TFPUPrecisionMode): TFPUPrecisionMode;
begin
  result:=pmDouble;
end;

procedure ClearExceptions(RaisePending: Boolean);
begin
end;

