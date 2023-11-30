{
    This file is part of the Free Pascal packages.
    Copyright (c) 2023 by the Free Pascal development team

    Implements a hash mechanism called FNV (see comment below)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit fnvhash;
{$ENDIF}

{$mode objfpc}

interface

uses 
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Types;
{$ELSE}
  sysutils, types;
{$ENDIF}

Const
  // FNV 32-bit hash, see http://www.isthe.com/chongo/src/fnv/hash_32.c
  FNV0_32_INIT = 0;
  FNV1_32_INIT = $811C9DC5;
  FNV_32_PRIME = $01000193;
  FNV0_64_INIT = 0;
  FNV1_64_INIT = QWord($84222325CBF29CE4);

  FNV_64_PRIME = QWord($100000001B3);


Type
  Fnv32_t = Cardinal;
  PFnv32_t = ^Fnv32_t;
  Fnv64_t = QWord;
  PFnv64_t = ^Fnv64_t;

function FNV1_32a(const Buf; BufLen: SizeInt; hVal: Fnv32_t): Fnv32_t; inline;
function FNV1_32a(const Buf : PByte; BufLen: SizeInt; hVal: Fnv32_t): Fnv32_t;
function FNV1_32(const Buf; BufLen: SizeInt; hVal: Fnv32_t): Fnv32_t; inline;
function FNV1_32(const Buf : PByte; BufLen: SizeInt; hVal: Fnv32_t): Fnv32_t;

function FNV1_64a(const Buf; BufLen: SizeInt; hVal: Fnv64_t): Fnv64_t; inline;
function FNV1_64a(const Buf : PByte; BufLen: SizeInt; hVal: Fnv64_t): Fnv64_t;
function FNV1_64(const Buf; BufLen: SizeInt; hVal: Fnv64_t): Fnv64_t; inline;
function FNV1_64(const Buf : PByte; BufLen: SizeInt; hVal: Fnv64_t): Fnv64_t;

implementation

{ *********************************************************************
  FNV 32-Bit
  *********************************************************************}


function FNV1_32a(const Buf: PByte; BufLen: SizeInt; hVal: Fnv32_t): Fnv32_t;

var
  BE,BP: PByte;

begin
  Result:=hVal;
  BP:=Buf;
  Be:=BP+BufLen;
  While (BP<BE) do
    begin
    Result:=Result xor Fnv32_t(BP^);
    Result:=Result*FNV_32_PRIME;
    Inc(BP);
    end;
end;

function FNV1_32a(const Buf; BufLen: SizeInt; hVal: Fnv32_t): Fnv32_t;

begin
  Result:=FNV1_32a(PByte(@Buf),BufLen,hVal);
end;

function FNV1_32(const Buf: PByte; BufLen: SizeInt; hVal: Fnv32_t): Fnv32_t;

var
  BE,BP: PByte;

begin
  Result:=hVal;
  BP:=Buf;
  Be:=BP+BufLen;
  While (BP<BE) do
    begin
    Result:=Result*FNV_32_PRIME;
    Result:=Result xor Fnv32_t(BP^);
    Inc(BP);
    end;
end;

function FNV1_32(const Buf; BufLen: SizeInt; hVal: Fnv32_t): Fnv32_t;

begin
  Result:=FNV1_32(PByte(@Buf),BufLen,hVal);
end;

{ *********************************************************************
  FNV 64-Bit
  *********************************************************************}


function FNV1_64a(const Buf: PByte; BufLen: SizeInt; hVal: Fnv64_t): Fnv64_t;

var
  BE,BP: PByte;

begin
  Result:=hVal;
  BP:=Buf;
  Be:=BP+BufLen;
  While (BP<BE) do
    begin
    Result:=Result xor Fnv64_t(BP^);
    Result:=Result*FNV_64_PRIME;
    Inc(BP);
    end;
end;

function FNV1_64a(const Buf; BufLen: SizeInt; hVal: Fnv64_t): Fnv64_t;

begin
  Result:=FNV1_64a(PByte(@Buf),BufLen,hVal);
end;

function FNV1_64(const Buf: PByte; BufLen: SizeInt; hVal: Fnv64_t): Fnv64_t;

var
  BE,BP: PByte;

begin
  Result:=hVal;
  BP:=Buf;
  Be:=BP+BufLen;
  While (BP<BE) do
    begin
    Result:=Result*FNV_64_PRIME;
    Result:=Result xor Fnv64_t(BP^);
    Inc(BP);
    end;
end;

function FNV1_64(const Buf; BufLen: SizeInt; hVal: Fnv64_t): Fnv64_t;

begin
  Result:=FNV1_64(PByte(@Buf),BufLen,hVal);
end;


end.

