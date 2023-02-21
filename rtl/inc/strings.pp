{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Strings unit for PAnsiChar (asciiz/C compatible strings) handling

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit Strings;
{$ENDIF FPC_DOTTEDUNITS}
{$S-}
{$inline on}
interface

    { Implemented in System Unit }
    function strpas(p:PAnsiChar):shortstring;inline;

    function strlen(p:PAnsiChar):sizeint;external name 'FPC_PCHAR_LENGTH';

    { Converts a Pascal string to a null-terminated string }
    function strpcopy(d : PAnsiChar;const s : shortstring) : PAnsiChar;

    { Copies source to dest, returns a pointer to dest }
    function strcopy(dest,source : PAnsiChar) : PAnsiChar; overload;

    { Copies at most maxlen bytes from source to dest. }
    { Returns a pointer to dest }
    function strlcopy(dest,source : PAnsiChar;maxlen : SizeInt) : PAnsiChar; overload;

    { Copies source to dest and returns a pointer to the terminating }
    { null character.    }
    function strecopy(dest,source : PAnsiChar) : PAnsiChar;

    { Returns a pointer tro the terminating null character of p }
    function strend(p : PAnsiChar) : PAnsiChar;

    { Appends source to dest, returns a pointer do dest}
    function strcat(dest,source : PAnsiChar) : PAnsiChar;

    { Compares str1 und str2, returns }
    { a value <0 if str1<str2;        }
    {  0 when str1=str2               }
    { and a value >0 if str1>str2     }
    function strcomp(str1,str2 : PAnsiChar) : SizeInt;

    { The same as strcomp, but at most l characters are compared  }
    function strlcomp(str1,str2 : PAnsiChar;l : SizeInt) : SizeInt;

    { The same as strcomp but case insensitive }
    function stricomp(str1,str2 : PAnsiChar) : SizeInt;

    { The same as stricomp, but at most l characters are compared }
    function strlicomp(str1,str2 : PAnsiChar;l : SizeInt) : SizeInt;

    { Copies l characters from source to dest, returns dest. }
    function strmove(dest,source : PAnsiChar;l : SizeInt) : PAnsiChar;

    { Appends at most l characters from source to dest }
    function strlcat(dest,source : PAnsiChar;l : SizeInt) : PAnsiChar;

    { Returns a pointer to the first occurrence of c in p }
    { If c doesn't occur, nil is returned }
    function strscan(p : PAnsiChar;c : AnsiChar) : PAnsiChar;

    { The same as strscan but case insensitive }
    function striscan(p : PAnsiChar;c : AnsiChar) : PAnsiChar;

    { Returns a pointer to the last occurrence of c in p }
    { If c doesn't occur, nil is returned }
    function strrscan(p : PAnsiChar;c : AnsiChar) : PAnsiChar;

    { The same as strrscan but case insensitive }
    function strriscan(p : PAnsiChar;c : AnsiChar) : PAnsiChar;

    { converts p to all-lowercase, returns p }
    function strlower(p : PAnsiChar) : PAnsiChar;

    { converts p to all-uppercase, returns p }
    function strupper(p : PAnsiChar) : PAnsiChar;

    { Returns a pointer to the first occurrence of str2 in }
    { str1 Otherwise returns nil }
    function strpos(str1,str2 : PAnsiChar) : PAnsiChar;

    { The same as strpos but case insensitive       }
    function stripos(str1,str2 : PAnsiChar) : PAnsiChar;

    { Makes a copy of p on the heap, and returns a pointer to this copy  }
    function strnew(p : PAnsiChar) : PAnsiChar;

    { Allocates L bytes on the heap, returns a PAnsiChar pointer to it }
    function stralloc(L : SizeInt) : PAnsiChar;

    { Releases a null-terminated string from the heap }
    procedure strdispose(p : PAnsiChar);

implementation

{$ifdef FPC_USE_LIBC}
{$i cgenstr.inc}
{$endif FPC_USE_LIBC}

{  Read Processor dependent part, shared with sysutils unit }
{$i strings.inc }

{ Read processor denpendent part, NOT shared with sysutils unit }
{$i stringss.inc }

{ Read generic string functions that are not implemented for the processor }
{$i genstr.inc}
{$i genstrs.inc}

{ Functions not in assembler, but shared with sysutils unit  }
{$i stringsi.inc}

{ Functions, different from the one in sysutils }


    procedure fpc_pchar_to_shortstr(var res : openstring;p:PAnsiChar);[external name 'FPC_PCHAR_TO_SHORTSTR'];


    function strpas(p:PAnsiChar):shortstring;{$ifdef SYSTEMINLINE}inline;{$endif}
      begin
        fpc_pchar_to_shortstr(strpas,p);
      end;

    function stralloc(L : SizeInt) : PAnsiChar;

      begin
         StrAlloc:=Nil;
         GetMem (Stralloc,l);
      end;

    function strnew(p : PAnsiChar) : PAnsiChar;

      var
         len : SizeInt;

      begin
         strnew:=nil;
         if (p=nil) or (p^=#0) then
           exit;
         len:=strlen(p)+1;
         getmem(strnew,len);
         if strnew<>nil then
           move(p^,strnew^,len);
      end;

    procedure strdispose(p : PAnsiChar);

      begin
         if p<>nil then
           freemem(p);
      end;

end.
