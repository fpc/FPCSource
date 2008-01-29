{
    *********************************************************************
    Copyright (C) 1997, 1998 Gertjan Schouten

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
    *********************************************************************

    System Utilities For Free Pascal
}

{  PChar functions  }

type
   CharArray = array[0..0] of char;

{ Processor dependent part, shared withs strings unit }
{$ifdef FPC_USE_LIBC}
{$i cgenstr.inc}
{$endif FPC_USE_LIBC}
{$i strings.inc }

{ Read generic string functions that are not implemented for the processor }
{$i genstr.inc}

{ Processor independent part, shared with strings unit }
{$i stringsi.inc }

{  StrPas converts a PChar to a pascal string  }

function StrPas(Str: PChar): string;
begin
  Result:=Str;
end ;

{  StrAlloc allocates a buffer of Size + 4
   the size of the allocated buffer is stored at result - 4
   StrDispose should be used to destroy the buffer  }

function StrAlloc(Size: cardinal): PChar;
begin
  inc(size,sizeof(cardinal));
  getmem(result,size);
  cardinal(pointer(result)^):=size;
  inc(result,sizeof(cardinal));
end;


{ Allocates a new string using StrAlloc, you need StrDispose to dispose the
  string }

function strnew(p : pchar) : pchar;
var
  len : longint;
begin
  Result:=nil;
  if (p=nil) or (p^=#0) then
   exit;
  len:=strlen(p)+1;
  Result:=StrAlloc(Len);
  if Result<>nil then
   strmove(Result,p,len);
end;


{  StrPCopy copies the pascal string Source to Dest and returns Dest  }

function StrPCopy(Dest: PChar; Source: string): PChar;
begin
  result := StrMove(Dest, PChar(Source), length(Source)+1);
end ;

{  StrPLCopy copies MaxLen or less characters from the pascal string
   Source to Dest and returns Dest  }

function StrPLCopy(Dest: PChar; Source: string; MaxLen: SizeUInt): PChar;
var Count: SizeUInt;
begin
result := Dest;
if (Result <> Nil) and (MaxLen <> 0) then begin
   Count := Length(Source);
   if Count > MaxLen then
      Count := MaxLen;
   StrMove(Dest, PChar(Source), Count);
   CharArray(result^)[Count] := #0;  { terminate ! }
   end ;
end ;


{   StrDispose clears the memory allocated with StrAlloc   }

procedure StrDispose(Str: PChar);
begin
  if (Str <> Nil) then
   begin
     dec(Str,sizeof(cardinal));
     Freemem(str,cardinal(pointer(str)^));
   end;
end;

{  StrBufSize returns the amount of memory allocated for pchar Str allocated with StrAlloc  }

function StrBufSize(Str: PChar): SizeUInt;
begin
  if Str <> Nil then
   result := SizeUInt(pointer(Str - SizeOf(SizeUInt))^)-sizeof(SizeUInt)
  else
   result := 0;
end ;

