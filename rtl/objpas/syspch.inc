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

function  NewStr(s:string):pchar;
var p:pchar;l:longint;
begin
l := length(s);
p := StrAlloc(l + 1);
move(s[1], p^, l);
byte(pchar(p + l)^) := 0;
NewStr := p;
end ;

function  StrAlloc(Size:longint):pchar;
var p:pointer;
begin
Getmem(p, size + sizeof(longint));
Move(Size, p^, sizeof(longint));
pbyte(p + sizeof(longint))^ := 0;
StrAlloc := pchar(p + sizeof(longint));
end ;

procedure StrDispose(var p:pchar);
var l:longint;
begin
if (p = nil) then exit;
p := pchar(p - sizeof(longint));
move(p^, l, sizeof(longint));
freemem(p, l + sizeof(longint));
p := nil;
end ;

function StrPas(p:pchar):string;
begin
   asm
      movl  P,%eax
      movl  %eax,%esi
      movl  __RESULT,%eax
      movl  %eax,%edi
      pushl %edi
      incl  %edi
      xorl  %eax,%eax
      movw  $255,%cx
   STR_LOOP1:
      lodsb
      orb   %al,%al
      jz    STR_END
      stosb
      loop  STR_LOOP1
   STR_END:
      popl  %edi
      movw  $255,%ax
      subw  %cx,%ax
      movb  %al,(%edi)
   end ;
end ;

function  StrLen(p:pchar):longint;
begin
	asm
      movl  p,%eax
      movl  %eax,%esi
      xorl  %eax,%eax
      movl  $0xFFFFFFFF,%ecx
   STRLEN_LOOP:
      incl  %ecx
      lodsb
      orb   %al,%al
      jnz   STRLEN_LOOP
      movl  %ecx,__RESULT
   end ;
end ;

function  StrEnd(p:pchar):pchar;
begin
   asm
      movl  p,%eax
      movl  %eax,%esi
   STREND_LOOP:
      lodsb
      orb   %al,%al
      jnz   STREND_LOOP
      movl  %esi,__RESULT
   end ;
end ;

function  StrMove(Dest, Source: PChar; Count: longint): PChar;
begin
   asm
      movl    source,%eax
      movl    %eax,%esi
      movl    dest,%eax
      movl    %eax,%edi
      movl    %edi,__RESULT
      movl    COUNT,%ecx
      movl    %ecx,%edx
      cmpl    %esi,%edi
      jg      STRMOVE_BACK
      shrl    $2,%ecx
      rep
		movsl
      movl    %edx,%ecx
      andl    $3,%ecx
      rep
		movsb
      jmp     STRMOVE_END
   STRMOVE_BACK:
      addl    %ecx,%edi
      decl    %edi
      addl    %ecx,%esi
      decl    %esi
      andl    $3,%ecx
      STD
      rep
		movsb
      subl    $3,%esi
      subl    $3,%edi
      movl    %edx,%ecx
      shrl    $2,%ecx
      rep
		movsl
      CLD
   STRMOVE_END:
   end ;
end ;

function  StrCat(Dest, Source: PChar): PChar;
begin
StrCat := Dest;
while char(dest^) <> #0 do
   dest := dest + 1;
while char(source^) <> #0 do begin
   char(dest^) := char(source^);
   dest := dest + 1;
   source := source + 1;
   end ;
char(dest^) := #0;
end ;

function  StrCat(Dest:pchar; Source: string): PChar;
var l:longint;
begin
StrCat := Dest;
while char(dest^) <> #0 do
   dest := dest + 1;
l := length(source);
move(source[1], dest^, l);
dest := dest + l;
char(dest^) := #0;
end ;

function  StrCat(var Dest:string; Source: pchar): String;
var count,l:longint;
begin
l := length(Dest);
count := setLength(Dest, l + StrLen(Source)) - l;
if (count > 0) then
   move(source^, dest[l + 1], count);
StrCat := Dest;
end ;

function  StrIns(Dest:pchar; Source: string): PChar;
var len:longint;
begin
len := length(Source);
StrMove(dest + len, dest, 1 + strlen(dest));
Move(Source[1], dest^, len);
StrIns := dest;
end ;

function  StrCopy(Dest, Source: PChar): Pchar;
begin
   asm
      movl    Dest,%eax
      movl    %eax,%edi
      movl    %eax,__RESULT
      movl    Source,%eax
      movl    %eax,%esi
   STRCOPY_LOOP:
      lodsb
      stosb
      orb     %al,%al
      jnz     STRCOPY_LOOP
   end ;
end ;

function  StrLCopy(Dest, Source: PChar; MaxLen: longint): PChar;
begin
   asm
      movl    Dest,%eax
      movl    %eax,__RESULT
      movl    %eax,%edi
      movl    Source,%eax
      movl    %eax,%esi
      movl    MaxLen,%ecx
      orl     %ecx,%ecx
      jz      STRLCOPY_END
   STRLCOPY_LOOP:
      lodsb
      orb     %al,%al
      jz      STRLCOPY_END
      stosb
      loop    STRLCOPY_LOOP
   STRLCOPY_END:
      xorb    %al,%al
      stosb
   end ;
end ;

function  StrScan(str:pchar;ch:char):pchar;
begin
   asm
      movl    str,%eax
      movl    %eax,%esi
      movb    ch,%bl
   STRSCAN_LOOP:
      lodsb
      cmpb    %al,%bl
      je      STRSCAN_END
      orb     %al,%al
      jnz     STRSCAN_LOOP
   STRSCAN_END:
      decl    %esi
      movl    %esi,__RESULT
   end ;
end ;

function  StrRScan(str:pchar;ch:char):pchar;
begin
   asm
      movl    str,%eax
      movl    %eax,%esi
      movl    %eax,%edx
      movb    ch,%bl
   STRRSCAN_LOOP:
      lodsb
      cmpb    %al,%bl
      jne     STRRSCAN_NOTFOUND
      movl    %esi,%edx
      decl    %edx
   STRRSCAN_NOTFOUND:
      orb     %al,%al
      jnz     STRRSCAN_LOOP
      movl    %edx,__RESULT
   end ;
end ;

function  StrTer(str:pchar;l:longint):pchar;
begin
   asm
   	movl    str,%eax
      movl    %eax,__RESULT
      addl    l,%eax
      movl    %eax,%edi
      xorb    %al,%al
      movb    %al,(%edi)
   end ;
end ;

{
  $Log$
  Revision 1.1  1998-04-10 15:17:46  michael
  + Initial implementation; Donated by Gertjan Schouten
    His file was split into several files, to keep it a little bit structured.

}