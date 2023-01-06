{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2016 by Marcus Sackrow and Karoly Balogh
    members of the Free Pascal development team.

    Command line parameter handling for Atari

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

(* define this symbol to get ARGV argument passing that's strictly
 * compatible with the Atari standard. If it's not defined, then
 * the startup code won't validate the ARGV= variable by checking
 * the command byte for 127. Note that there are still some
 * applications (gulam is a notable example) that implement only
 * part of the standard and don't set the command byte to 127.
 *)
{$IF 0}
{$DEFINE STRICTLY_COMPATIBLE_WITH_STANDARD }
{$ENDIF}

var execpathstr : shortstring;

{ Generates correct argument array on startup }
procedure GenerateArgs;
var
  ArgVLen: LongInt;
  LocalIndex: Word;
  len: Integer;

  procedure AllocArg(Idx, Len: LongInt);
  var
    i, OldArgVLen : LongInt;
  begin
    if Idx >= ArgVLen then
    begin
      OldArgVLen := ArgVLen;
      ArgVLen := (Idx + 8) and (not 7);
      SysReAllocMem(Argv, Argvlen * SizeOf(Pointer));
      for i := OldArgVLen to ArgVLen - 1 do
        ArgV[i]:=nil;
    end;
    ArgV[Idx] := SysAllocMem(Succ(Len));
  end;

  function scan_argv : boolean;
  var
     hp, start : PAnsiChar;
     len: integer;
  begin
   hp:=basepage^.p_env;
   result:=false;
   if (hp=nil) then
      exit;
   LocalIndex := 0;
   while hp^<>#0 do
     begin
       if (hp[0] = 'A') and (hp[1] = 'R') and (hp[2] = 'G') and (hp[3] = 'V') and (hp[4] = '=') then
       begin
         { in any case, terminate environment here }
         hp[0] := #0;
         hp[1] := #0;
         { skip ARGV= string }
         hp := hp + 5;
         if (hp[0] = 'N') and (hp[1] = 'U') and (hp[2] = 'L') and (hp[3] = 'L') and (hp[4] = ':') then
           begin
             { TODO: handle NULL arguments }
           end;
{$ifdef STRICTLY_COMPATIBLE_WITH_STANDARD}
         if (len <> 127) then
           exit;
{$endif}
         { skip ARGV= value }
         while hp^<>#0 do
           inc(hp);
         inc(hp);
         { get arguments }
         while hp^<>#0 do
           begin
             start := hp;
             while hp^<>#0 do
               inc(hp);
             len := hp - start;
             allocarg(localindex,len);
             move(start^,argv[localindex]^,len);
             argv[localindex][len]:=#0;
             inc(localindex);
             inc(hp);
           end;
         argc:=localindex;
         result := true;
         exit;
       end;
       hp := hp + strlen(hp) + 1;
     end;
   end;

var
  Count: Word;
  Start: Word;
  Ende: Word;
  i: Integer;
  P : PAnsiChar;
begin
  P := Args;
  ArgVLen := 0;

  { check ARGV usage indicator }
  len := ord(P[0]);
  if scan_argv then
    exit;

  { Set argv[0] }
  AllocArg(0, 0);
  Argv[0][0] := #0;

  { just in case; commandline cannot be longer }
  if len > 127 then
    begin
      argc := 1;
      exit;
    end;

  { Handle the other args }
  p[len + 1] := #0;
  Count := 1;
  { first index is one }
  LocalIndex := 1;
  while (P[Count] <> #0) do
  begin
    while (P[Count] <> #0) and (p[count]<=#32) do
      Inc(count);
    if p[count] = '"' then
    begin
      Inc(Count);
      start := count;
      while (p[count]<>#0) and (p[count]<>'"') and (p[count]>=#32) do
        Inc(Count);
      ende := count;
      if (p[count] = '"') then
        Inc(Count);
    end else
    begin
      start := count;
      while (p[count]<>#0) and (p[count]>#32) do
        inc(count);
      ende := count;
    end;
    if (ende>start) then
    begin
      allocarg(localindex,ende-start);
      move(p[start],argv[localindex]^,ende-start);
      argv[localindex][ende-start]:=#0;
      inc(localindex);
    end;
  end;
  argc:=localindex;
end;


Function FSearch(const path:RawByteString;dirlist:RawByteString):RawByteString;
{
  Searches for a file 'path' in the list of direcories in 'dirlist'.
  returns an empty string if not found. Wildcards are NOT allowed.
  If dirlist is empty, it is set to '.'

This function tries to make FSearch use ansistrings, and decrease
stringhandling overhead at the same time.

}
Var
  mypath,
  mydir,NewDir : RawByteString;
  p1     : longint;
  olddta : PDTA;
  dta    : TDTA;
  i,j    : longint;
  p      : PAnsiChar;
  tmpPath: RawByteString;
Begin

{Check for WildCards}
  If (Pos('?',Path) <> 0) or (Pos('*',Path) <> 0) Then
   FSearch:='' {No wildcards allowed in these things.}
  Else
   Begin
     { allow slash as backslash }
     tmpPath:=Path+#0;
     DoDirSeparators(tmpPath);
     DoDirSeparators(dirlist);
     {Replace ';' with #0}
     for p1:=1 to length(dirlist) do
       if (dirlist[p1]=';') or (dirlist[p1]=',') then
         dirlist[p1]:=#0;

     mypath:=ToSingleByteFileSystemEncodedFileName(tmppath);
     olddta := gemdos_getdta;
     gemdos_setdta(@dta);
     p:=PAnsiChar(dirlist);
     i:=length(dirlist);
     j:=1;
     Repeat
       mydir:=RawByteString(p);
       if (length(mydir)>0) and (mydir[length(mydir)]<>DirectorySeparator) then
          begin
            { concatenate character without influencing code page }
            setlength(mydir,length(mydir)+1);
            mydir[length(mydir)]:=DirectorySeparator;
          end;
       NewDir:=mydir+mypath;
       if (gemdos_fsfirst(PAnsiChar(NewDir),$07)>=0) and
          ((dta.d_attrib and ATTRIB_DIRECTORY)=0) then
        Begin
          {DOS strips off an initial .\}
          If Pos('.\',NewDir)=1 Then
            Delete(NewDir,1,2);
        End
       Else
        NewDir:='';
       while (j<=i) and (p^<>#0) do begin inc(j); inc(p); end;
       if p^=#0 then inc(p);
     Until (j>=i) or (Length(NewDir) > 0);
     gemdos_setdta(olddta);
     FSearch:=NewDir;
   End;
End;


{*****************************************************************************
                             ParamStr
*****************************************************************************}

{ number of args }
function ParamCount: LongInt;
begin
  ParamCount := argc - 1;
end;

function fpGetEnvAtari(const envvar : ShortString): RawByteString; external name '_fpc_atari_getenv';

{ argument number l }
function ParamStr(l: LongInt): shortstring;
var
  s1: shortstring;
begin
  if l=0 then
    begin
      if (execpathstr='') and (argv[0][0]<>#0) then
        begin
          execpathstr := fsearch(argv[0],fpgetenvAtari('PATH'));
          if execpathstr='' then
            execpathstr := argv[0];
        end;
      paramstr := execpathstr;
    end
  else if (l > 0) and (l < argc) then
    ParamStr := StrPas(argv[l])
  else
    ParamStr := '';
end;
