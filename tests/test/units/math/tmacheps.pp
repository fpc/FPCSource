{ Machine epsilon constants in Math and the float helpers, issue #39995 }
program tmacheps;

{$mode objfpc}{$h+}

uses
  SysUtils, Math;

var
{$ifdef FPC_HAS_TYPE_SINGLE}
  lS, lSOne: Single;
{$endif}
{$ifdef FPC_HAS_TYPE_DOUBLE}
  lD, lDOne: Double;
{$endif}
  lE, lEOne: Extended;
  lF, lFOne: Float;

begin
{$ifdef FPC_HAS_TYPE_SINGLE}
  if Single.MachineEpsilon<>MachEpsSingle then
    halt(1);
  lS:=MachEpsSingle;
  if PLongWord(@lS)^<>$34000000 then
    halt(2);
  lSOne:=1.0;
  lS:=lSOne+MachEpsSingle;
  if lS=lSOne then
    halt(3);
  lS:=lSOne+MachEpsSingle/2;
  if lS<>lSOne then
    halt(4);
{$endif}

{$ifdef FPC_HAS_TYPE_DOUBLE}
  if Double.MachineEpsilon<>MachEpsDouble then
    halt(11);
  lD:=MachEpsDouble;
  if PQWord(@lD)^<>QWord($3CB0000000000000) then
    halt(12);
  lDOne:=1.0;
  lD:=lDOne+MachEpsDouble;
  if lD=lDOne then
    halt(13);
  lD:=lDOne+MachEpsDouble/2;
  if lD<>lDOne then
    halt(14);
{$endif}

  if Extended.MachineEpsilon<>MachEpsExtended then
    halt(21);
{$if defined(FPC_HAS_TYPE_EXTENDED) and (sizeof(extended)<>sizeof(double))}
  lE:=MachEpsExtended;
  if (TExtended80Rec(lE)._Exp<>$3FC0) or (TExtended80Rec(lE).Frac<>QWord($8000000000000000)) then
    halt(22);
{$endif}
  lEOne:=1.0;
  lE:=lEOne+MachEpsExtended;
  if lE=lEOne then
    halt(23);
  lE:=lEOne+MachEpsExtended/2;
  if lE<>lEOne then
    halt(24);

  lFOne:=1.0;
  lF:=lFOne+MachEpsFloat;
  if lF=lFOne then
    halt(31);
  lF:=lFOne+MachEpsFloat/2;
  if lF<>lFOne then
    halt(32);
  writeln('ok');
end.
