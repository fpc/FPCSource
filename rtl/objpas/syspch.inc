{
    *********************************************************************
    $Id$
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
   pbyte = ^byte;
   CharArray = array[0..0] of char;

{ Processor dependent part, shared withs strings unit }
{$i strings.inc }

{ Processor independent part, shared with strings unit }
{$i stringsi.inc }

{  StrPas converts a PChar to a pascal string  }

function StrPas(Str: PChar): string;
begin
  SetLength(result, StrLen(Str));
  Move(Str^, result[1], Length(result));
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
  strnew:=nil;
  if (p=nil) or (p^=#0) then
   exit;
  len:=strlen(p)+1;
  StrNew:=StrAlloc(Len);
  if strnew<>nil then
   strmove(strnew,p,len);
end;


{  StrPCopy copies the pascal string Source to Dest and returns Dest  }

function StrPCopy(Dest: PChar; Source: string): PChar;
begin
  result := StrMove(Dest, PChar(Source), length(Source)+1);
end ;

{  StrPLCopy copies MaxLen or less characters from the pascal string
   Source to Dest and returns Dest  }

function StrPLCopy(Dest: PChar; Source: string; MaxLen: cardinal): PChar;
var Count: cardinal;
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

function StrBufSize(Str: PChar): cardinal;
begin
  if Str <> Nil then
   result := cardinal(pointer(Str - SizeOf(cardinal))^)-sizeof(cardinal)
  else
   result := 0;
end ;

{
  $Log$
  Revision 1.1  2000-07-13 06:31:01  michael
  + Initial import

  Revision 1.9  2000/02/09 16:59:32  peter
    * truncated log

  Revision 1.8  1999/12/10 15:02:12  peter
    * strnew is ofcourse also different between sysutils and strings, just
      like stralloc/strdispose.

  Revision 1.7  1999/11/06 14:41:31  peter
    * truncated log

  Revision 1.6  1999/08/24 13:14:50  peter
    * disposestr allocstr compatible with delphi

}
