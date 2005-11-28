{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 Karoly Balogh for Genesi S.a.r.l. <www.genesi.lu>

    utility.library interface unit for MorphOS/PowerPC

    MorphOS port was done on a free Pegasos II/G4 machine
    provided by Genesi S.a.r.l. <www.genesi.lu>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$inline on}
unit utility;

interface

uses
  exec;

var
  UtilityBase: Pointer;

{$include utild1.inc}
{$include utild2.inc}
{$include utilf.inc}

function TAG(value: pointer): longword; inline;
function TAG(value: pchar): longword; inline;


implementation

function TAG(value: pointer): longword; inline;
begin
  TAG:=longword(value);
end;

function TAG(value: pchar): longword; inline;
begin
  TAG:=longword(value);
end;


begin
  UtilityBase:=MOS_UtilityBase;
end.
