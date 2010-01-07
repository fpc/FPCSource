{
    Driver Development Kit for Native NT

    This file is part of the Free Pascal run time library.
    Copyright (c) 2009 by Sven Barth

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit DDK;

interface

uses
  NDK;

const
  // we distinguish the user- AND kernel-mode imports (NDK.ntdll) from the pure
  // kernel mode imports (ntkrnl)
  ntkrnl = 'ntoskrnl.exe';

{$include ddktypes.inc}

// these two only return not Nil in main routine of a device driver
function RegistryPath: PNtUnicodeString; inline;
function DriverObject: PDriverObject; inline;

function DbgPrint(aFormat: PChar): LongWord; cdecl; varargs; external ntkrnl name 'DbgPrint';

function PoolTag(aTag: TTagString): LongWord;

{$include ddkex.inc}

implementation

function RegistryPath: PNtUnicodeString; inline;
begin
  RegistryPath := SysRegistryPath;
end;

function DriverObject: PDriverObject; inline;
begin
  DriverObject := SysDriverObject;
end;

function PoolTag(aTag: TTagString): LongWord;
begin
  PoolTag := Ord(aTag[1]) + Ord(aTag[2]) shl 8 +
         Ord(aTag[3]) shl 16 + Ord(aTag[4]) shl 24;
end;

end.

