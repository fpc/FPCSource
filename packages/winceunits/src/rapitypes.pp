{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2009 Sven Barth.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }

{
  Types from rapitypes.h, others are directly in rapi.pp, but this unit is used
  from Win32 and WinCE.
}
unit RAPITypes;

{$mode objfpc}{$H+}

interface

uses
  types;

type
  RAPISTREAMFLAG = (
    STREAM_TIMEOUT_READ
  );
  TRapiStreamFlag = RAPISTREAMFLAG;

  IRAPIStream = interface(IStream)
    function SetRapiStat(Flag: RAPISTREAMFLAG; dwValue: DWord): HResult; stdcall;
    function GetRapiStat(Flag: RAPISTREAMFLAG; pdwValue: PDWord): HResult; stdcall;
  end;
  PIRAPIStream = ^IRAPIStream;

implementation

end.

