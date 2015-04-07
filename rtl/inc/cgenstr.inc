{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Carl-Eric Codere,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ we have to call the libc routines, because simply declaring our routines }
{ as cdecl and external in libc cause problems because the calling         }
{ convention the interface is different                                    }

{$ifndef FPC_UNIT_HAS_STREND}
{$define FPC_UNIT_HAS_STREND}

 function StrEnd(P: PChar): PChar;{$ifdef SYSTEMINLINE}inline;{$endif}
   begin
     strend := p+strlen(p);
   end;
{$endif FPC_UNIT_HAS_STREND}


{$ifndef FPC_UNIT_HAS_STRCOPY}
{$define FPC_UNIT_HAS_STRCOPY}
 function libc_strcpy(dest: pchar; const src: pchar): pchar; cdecl; external 'c' name 'strcpy';

 Function StrCopy(Dest, Source:PChar): PChar;{$ifdef SYSTEMINLINE}inline;{$endif}
   Begin
     StrCopy := libc_strcpy(dest,source);
   end;
{$endif FPC_UNIT_HAS_STRCOPY}


{$ifndef FPC_UNIT_HAS_STRSCAN}
{$define FPC_UNIT_HAS_STRSCAN}
 function libc_strchr(const p: pchar; c: longint): pchar; cdecl; external 'c' name 'strchr';

 function StrScan(P: PChar; C: Char): PChar;{$ifdef SYSTEMINLINE}inline;{$endif}
   Begin
     StrScan := libc_strchr(p,longint(c));
   end;
{$endif FPC_UNIT_HAS_STRSCAN}


{$ifndef FPC_UNIT_HAS_STRRSCAN}
{$define FPC_UNIT_HAS_STRRSCAN}
 function libc_strrchr(const p: pchar; c: longint): pchar; cdecl; external 'c' name 'strrchr';

 function StrRScan(P: PChar; C: Char): PChar;{$ifdef SYSTEMINLINE}inline;{$endif}
   Begin
     StrRScan := libc_strrchr(p,longint(c));
   end;
{$endif FPC_UNIT_HAS_STRRSCAN}

(*
{$ifndef FPC_UNIT_HAS_STRECOPY}
{$define FPC_UNIT_HAS_STRECOPY}
 function libc_stpcpy(dest: pchar; const src: pchar): pchar; cdecl; external 'c' name 'stpcpy';

  Function StrECopy(Dest, Source: PChar): PChar;{$ifdef SYSTEMINLINE}inline;{$endif}
    Begin
      StrECopy := libc_stpcpy(dest,source);
    end;
{$endif FPC_UNIT_HAS_STRECOPY}
*)

(*
{$ifndef FPC_UNIT_HAS_STRLCOPY}
{$define FPC_UNIT_HAS_STRLCOPY}

 function libc_strlcpy(dest: pchar; const src: pchar; maxlen: SizeInt): SizeInt; cdecl; external 'c' name 'strlcpy';

 Function StrLCopy(Dest,Source: PChar; MaxLen: SizeInt): PChar;{$ifdef SYSTEMINLINE}inline;{$endif}
   Begin
     libc_strlcpy(dest,source,maxlen);
     StrLCopy := Dest;
   end;
{$endif FPC_UNIT_HAS_STRLCOPY}
*)

{$ifndef FPC_UNIT_HAS_STRCOMP}
{$define FPC_UNIT_HAS_STRCOMP}
 function libc_strcmp(const str1,str2: pchar): longint; cdecl; external 'c' name 'strcmp';

 function StrComp(Str1, Str2 : PChar): SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
   Begin
     strcomp := libc_strcmp(str1,str2);
   end;
{$endif FPC_UNIT_HAS_STRCOMP}


{$ifndef FPC_UNIT_HAS_STRICOMP}
{$define FPC_UNIT_HAS_STRICOMP}
 function libc_strcasecmp(const str1,str2: pchar): longint; cdecl; external 'c' name 'strcasecmp';

 function StrIComp(Str1, Str2 : PChar): SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
   Begin
     stricomp := libc_strcasecmp(str1,str2);
   end;
{$endif FPC_UNIT_HAS_STRICOMP}


{$ifndef FPC_UNIT_HAS_STRLCOMP}
{$define FPC_UNIT_HAS_STRLCOMP}
 function libc_strncmp(const str1,str2: pchar; l: sizeint): longint; cdecl; external 'c' name 'strncmp';

 function StrLComp(Str1, Str2 : PChar; L: SizeInt): SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
   Begin
     strlcomp := libc_strncmp(str1,str2,l);
   end;
{$endif FPC_UNIT_HAS_STRLCOMP}


{$ifndef FPC_UNIT_HAS_STRLICOMP}
{$define FPC_UNIT_HAS_STRLICOMP}
 function libc_strncasecmp(const str1,str2: pchar; l: sizeint): longint; cdecl; external 'c' name 'strncasecmp';

 function StrLIComp(Str1, Str2 : PChar; L: SizeInt): SizeInt;{$ifdef SYSTEMINLINE}inline;{$endif}
   Begin
     strlicomp := libc_strncasecmp(str1,str2,l);
   end;
{$endif FPC_UNIT_HAS_STRLICOMP}


