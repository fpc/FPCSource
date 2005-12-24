{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Processor independent implementation for the system unit
    (based on libc)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                               Primitives
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_MOVE}
{$define FPC_SYSTEM_HAS_MOVE}
procedure bcopy(const source;var dest;count:sizeuint); cdecl; external 'c' name 'bcopy';

{ we need this separate move declaration because we can't add a "public, alias" to the above }
procedure Move(const source;var dest;count:sizeint); [public, alias: 'FPC_MOVE'];{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  if count <= 0 then
    exit;
  bcopy(source,dest,count);
end;
{$endif not FPC_SYSTEM_HAS_MOVE}


{$ifndef FPC_SYSTEM_HAS_FILLCHAR}
{$define FPC_SYSTEM_HAS_FILLCHAR}
procedure memset(var x; value: byte; count: sizeuint); cdecl; external 'c';

Procedure FillChar(var x;count: sizeint;value:byte);{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  if count <= 0 then
    exit;
  memset(x,value,count);
end;
{$endif FPC_SYSTEM_HAS_FILLCHAR}


{$ifndef FPC_SYSTEM_HAS_FILLBYTE}
{$define FPC_SYSTEM_HAS_FILLBYTE}
procedure FillByte (var x;count : sizeint;value : byte );{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  if count <= 0 then
    exit;
  FillChar (X,Count,value);
end;
{$endif not FPC_SYSTEM_HAS_FILLBYTE}


{$ifndef FPC_SYSTEM_HAS_INDEXCHAR}
{$define FPC_SYSTEM_HAS_INDEXCHAR}

function memchr(const buf; b: sizeuint; len: cardinal): pointer; cdecl; external 'c';

function IndexChar(Const buf;len:sizeint;b:char):sizeint;
var
  res: pointer;
begin
  if len = 0 then
    exit(-1);
  { simulate assembler implementations behaviour, which is expected }
  { fpc_pchar_to_ansistr in astrings.inc (interpret values < 0 as   }
  { unsigned)                                                       }
  res := memchr(buf,longint(b),cardinal(len));
  if (res <> nil) then
    IndexChar := SizeInt(res-@buf)
  else
    IndexChar := -1;
end;
{$endif not FPC_SYSTEM_HAS_INDEXCHAR}


{$ifndef FPC_SYSTEM_HAS_INDEXBYTE}
{$define FPC_SYSTEM_HAS_INDEXBYTE}
function IndexByte(Const buf;len:sizeint;b:byte):sizeint;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  IndexByte:=IndexChar(buf,len,char(b));
end;
{$endif not FPC_SYSTEM_HAS_INDEXBYTE}


{$ifndef FPC_SYSTEM_HAS_COMPARECHAR}
{$define FPC_SYSTEM_HAS_COMPARECHAR}
function memcmp_comparechar(Const buf1,buf2;len:sizeuint):longint; cdecl; external 'c' name 'memcmp';

function CompareChar(Const buf1,buf2;len:sizeint):sizeint;
var
  res: longint;
begin
  if len <= 0 then
    exit(0);
  res := memcmp_comparechar(buf1,buf2,len);
  if res < 0 then
    CompareChar := -1
  else if res > 0 then
    CompareChar := 1
  else
    CompareChar := 0;
end;
{$endif not FPC_SYSTEM_HAS_COMPARECHAR}


{$ifndef FPC_SYSTEM_HAS_COMPAREBYTE}
{$define FPC_SYSTEM_HAS_COMPAREBYTE}
function CompareByte(Const buf1,buf2;len:sizeint):sizeint;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  CompareByte := CompareChar(buf1,buf2,len);
end;
{$endif not FPC_SYSTEM_HAS_COMPAREBYTE}


{$ifndef FPC_SYSTEM_HAS_COMPARECHAR0}
{$define FPC_SYSTEM_HAS_COMPARECHAR0}
function strncmp_comparechar0(Const buf1,buf2;len:sizeuint):longint; cdecl; external 'c' name 'strncmp';

function CompareChar0(Const buf1,buf2;len:sizeint):sizeint;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  if len <= 0 then
    exit(0);
  strncmp_comparechar0(buf1,buf2,len);
end;

{$endif not FPC_SYSTEM_HAS_COMPARECHAR0}



{$ifndef FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}
{$define FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}

function libc_pchar_length(p:pchar):sizeuint; cdecl; external 'c' name 'strlen';

function fpc_pchar_length(p:pchar):longint;[public,alias:'FPC_PCHAR_LENGTH']; compilerproc;
begin
  fpc_pchar_length:=libc_pchar_length(p);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}


