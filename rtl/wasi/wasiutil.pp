{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2021 by the Free Pascal development team.

    Helper RTL functions for The WebAssembly System Interface (WASI).

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit wasiutil;

{$mode objfpc}

interface

uses
  wasiapi;

type
  PWasiSearchRec = ^TWasiSearchRec;
  TWasiSearchRec = record
    SearchPos  : UInt64;             {directory position}
    SearchNum  : LongInt;            {to track which search this is}
    DirFD      : __wasi_fd_t;        {directory fd handle for reading directory}
    SearchType : Byte;               {0=normal, 1=open will close, 2=only 1 file}
    SearchAttr : Byte;               {attribute we are searching for}
    Attr       : Byte;               {attribute of found file}
    Time       : __wasi_timestamp_t; {last modify date of found file}
    Size       : __wasi_filesize_t;  {file size of found file}
    Name       : RawByteString;      {name of found file}
    SearchSpec : RawByteString;      {search pattern}
    NamePos    : Word;               {end of path, start of name position}
  End;

function ConvertToFdRelativePath(path: RawByteString; out fd: LongInt; out relfd_path: RawByteString): Word; external name 'FPC_WASI_CONVERTTOFDRELATIVEPATH';
function fpc_wasi_path_readlink_ansistring(fd: __wasi_fd_t; const path: PAnsiChar; path_len: size_t; out link: rawbytestring): __wasi_errno_t; external name 'FPC_WASI_PATH_READLINK_ANSISTRING';
function FNMatch(const Pattern,Name:rawbytestring):Boolean;
function WasiFindFirst(const Path: RawByteString; Attr: Word; var f: TWasiSearchRec): longint;
function WasiFindNext(var f: TWasiSearchRec): longint;
procedure WasiFindClose(var f: TWasiSearchRec);
Function UniversalToEpoch(year,month,day,hour,minute,second:Word):int64;
Function LocalToEpoch(year,month,day,hour,minute,second:Word):int64;
Procedure EpochToUniversal(epoch:int64;var year,month,day,hour,minute,second:Word);
Procedure EpochToLocal(epoch:int64;var year,month,day,hour,minute,second:Word);

implementation

const
  {Bitmasks for file attribute}
  readonly  = $01;
  hidden    = $02;
  sysfile   = $04;
  volumeid  = $08;
  directory = $10;
  archive   = $20;
  anyfile   = $3F;

Const
  RtlFindSize = 15;
Type
  RtlFindRecType = Record
    DirFD    : LongInt;
    SearchNum,
    LastUsed : LongInt;
  End;
Var
  RtlFindRecs   : Array[1..RtlFindSize] of RtlFindRecType;
  CurrSearchNum : LongInt;

Function FNMatch(const Pattern,Name:rawbytestring):Boolean;
Var
  LenPat,LenName : longint;

  Function DoFNMatch(i,j:longint):Boolean;
  Var
    Found : boolean;
  Begin
  Found:=true;
  While Found and (i<=LenPat) Do
   Begin
     Case Pattern[i] of
      '?' : Found:=(j<=LenName);
      '*' : Begin
            {find the next character in pattern, different of ? and *}
              while Found do
                begin
                inc(i);
                if i>LenPat then Break;
                case Pattern[i] of
                  '*' : ;
                  '?' : begin
                          if j>LenName then begin DoFNMatch:=false; Exit; end;
                          inc(j);
                        end;
                else
                  Found:=false;
                end;
               end;
              Assert((i>LenPat) or ( (Pattern[i]<>'*') and (Pattern[i]<>'?') ));
            {Now, find in name the character which i points to, if the * or ?
             wasn't the last character in the pattern, else, use up all the
             chars in name}
              Found:=false;
              if (i<=LenPat) then
              begin
                repeat
                  {find a letter (not only first !) which maches pattern[i]}
                  while (j<=LenName) and (name[j]<>pattern[i]) do
                    inc (j);
                  if (j<LenName) then
                  begin
                    if DoFnMatch(i+1,j+1) then
                    begin
                      i:=LenPat;
                      j:=LenName;{we can stop}
                      Found:=true;
                      Break;
                    end else
                      inc(j);{We didn't find one, need to look further}
                  end else
                  if j=LenName then
                  begin
                    Found:=true;
                    Break;
                  end;
                  { This 'until' condition must be j>LenName, not j>=LenName.
                    That's because when we 'need to look further' and
                    j = LenName then loop must not terminate. }
                until (j>LenName);
              end else
              begin
                j:=LenName;{we can stop}
                Found:=true;
              end;
            end;
     else {not a wildcard character in pattern}
       Found:=(j<=LenName) and (pattern[i]=name[j]);
     end;
     inc(i);
     inc(j);
   end;
  DoFnMatch:=Found and (j>LenName);
  end;

Begin {start FNMatch}
  LenPat:=Length(Pattern);
  LenName:=Length(Name);
  FNMatch:=DoFNMatch(1,1);
End;

Procedure WasiFindClose(Var f: TWasiSearchRec);
{
  Closes dirfd if it is open
}
Var
  res: __wasi_errno_t;
  i : longint;
Begin
  if f.SearchType=0 then
   begin
     i:=1;
     repeat
       if (RtlFindRecs[i].SearchNum=f.SearchNum) then
        break;
       inc(i);
     until (i>RtlFindSize);
     If i<=RtlFindSize Then
      Begin
        RtlFindRecs[i].SearchNum:=0;
        if f.dirfd<>-1 then
          repeat
            res:=__wasi_fd_close(f.dirfd);
          until (res=__WASI_ERRNO_SUCCESS) or (res<>__WASI_ERRNO_INTR);
      End;
   end;
  f.dirfd:=-1;
End;


Function FindGetFileInfo(const s:rawbytestring;var f:TWasiSearchRec):boolean;
var
  st   : __wasi_filestat_t;
  fd   : __wasi_fd_t;
  pr   : RawByteString;
  Info : record
    FMode: LongInt;
    FSize: __wasi_filesize_t;
    FMTime: __wasi_timestamp_t;
  end;
begin
  FindGetFileInfo:=false;
  if ConvertToFdRelativePath(s,fd,pr)<>0 then
    exit;
  { todo: __WASI_LOOKUPFLAGS_SYMLINK_FOLLOW??? }
  if __wasi_path_filestat_get(fd,0,PAnsiChar(pr),Length(pr),@st)<>__WASI_ERRNO_SUCCESS then
    exit;
  info.FSize:=st.size;
  info.FMTime:=st.mtim;
  if st.filetype=__WASI_FILETYPE_DIRECTORY then
   info.fmode:=$10
  else
   info.fmode:=$0;
  {if (st.st_mode and STAT_IWUSR)=0 then
   info.fmode:=info.fmode or 1;}
  if s[f.NamePos+1]='.' then
   info.fmode:=info.fmode or $2;

  If ((Info.FMode and Not(f.searchattr))=0) Then
   Begin
     f.Name:=Copy(s,f.NamePos+1);
     f.Attr:=Info.FMode;
     f.Size:=Info.FSize;
     f.Time:=Info.FMTime;
     FindGetFileInfo:=true;
   End;
end;


Function  FindLastUsed: Longint;
{
  Find unused or least recently used dirpointer slot in findrecs array
}
Var
  BestMatch,i : Longint;
  Found       : Boolean;
Begin
  BestMatch:=1;
  i:=1;
  Found:=False;
  While (i <= RtlFindSize) And (Not Found) Do
   Begin
     If (RtlFindRecs[i].SearchNum = 0) Then
      Begin
        BestMatch := i;
        Found := True;
      End
     Else
      Begin
        If RtlFindRecs[i].LastUsed > RtlFindRecs[BestMatch].LastUsed Then
         BestMatch := i;
      End;
     Inc(i);
   End;
  FindLastUsed := BestMatch;
End;



function WasiFindNext(var f: TWasiSearchRec): longint;
{
  re-opens dir if not already in array and calls FindWorkProc
}
Var
  fd,ourfd: __wasi_fd_t;
  pr: RawByteString;
  res: __wasi_errno_t;
  DirName  : RawByteString;
  i,
  ArrayPos : Longint;
  FName,
  SName    : RawByteString;
  Found,
  Finished : boolean;
  Buf: array [0..SizeOf(__wasi_dirent_t)+256-1] of Byte;
  BufUsed: __wasi_size_t;
Begin
  If f.SearchType=0 Then
   Begin
     ArrayPos:=0;
     For i:=1 to RtlFindSize Do
      Begin
        If RtlFindRecs[i].SearchNum = f.SearchNum Then
         ArrayPos:=i;
        Inc(RtlFindRecs[i].LastUsed);
      End;
     If ArrayPos=0 Then
      Begin
        If f.NamePos = 0 Then
         DirName:='./'
        Else
         DirName:=Copy(f.SearchSpec,1,f.NamePos);
        if ConvertToFdRelativePath(DirName,fd,pr)=0 then
         begin
           repeat
             res:=__wasi_path_open(fd,
                                   0,
                                   PAnsiChar(pr),
                                   length(pr),
                                   __WASI_OFLAGS_DIRECTORY,
                                   __WASI_RIGHTS_FD_READDIR,
                                   __WASI_RIGHTS_FD_READDIR,
                                   0,
                                   @ourfd);
           until (res=__WASI_ERRNO_SUCCESS) or (res<>__WASI_ERRNO_INTR);
           If res=__WASI_ERRNO_SUCCESS Then
            begin
              f.DirFD := ourfd;
              ArrayPos:=FindLastUsed;
              If RtlFindRecs[ArrayPos].SearchNum > 0 Then
                repeat
                  res:=__wasi_fd_close(RtlFindRecs[arraypos].DirFD);
                until (res=__WASI_ERRNO_SUCCESS) or (res<>__WASI_ERRNO_INTR);
              RtlFindRecs[ArrayPos].SearchNum := f.SearchNum;
              RtlFindRecs[ArrayPos].DirFD := f.DirFD;
            end
           else
            f.DirFD:=-1;
         end
        else
         f.DirFD:=-1;
      End;
     if ArrayPos>0 then
       RtlFindRecs[ArrayPos].LastUsed:=0;
   end;
{Main loop}
  SName:=Copy(f.SearchSpec,f.NamePos+1);
  Found:=False;
  Finished:=(f.DirFD=-1);
  While Not Finished Do
   Begin
     res:=__wasi_fd_readdir(f.DirFD,
                            @buf,
                            SizeOf(buf),
                            f.searchpos,
                            @bufused);
     if (res<>__WASI_ERRNO_SUCCESS) or (bufused<=SizeOf(__wasi_dirent_t)) then
      FName:=''
     else
      begin
        SetLength(FName,P__wasi_dirent_t(@buf)^.d_namlen);
        Move(buf[SizeOf(__wasi_dirent_t)],FName[1],Length(FName));
        f.searchpos:=P__wasi_dirent_t(@buf)^.d_next;
      end;
     If FName='' Then
      Finished:=True
     Else
      Begin
        If FNMatch(SName,FName) Then
         Begin
           Found:=FindGetFileInfo(Copy(f.SearchSpec,1,f.NamePos)+FName,f);
           if Found then
            Finished:=true;
         End;
      End;
   End;
{Shutdown}
  If Found Then
   result:=0
  Else
   Begin
     WasiFindClose(f);
     result:=18;
   End;
End;


function WasiFindFirst(const Path: RawByteString; Attr: Word; var f: TWasiSearchRec): longint;
{
  opens dir and calls FindWorkProc
}
Begin
  fillchar(f,sizeof(f),0);
  if Path='' then
   begin
     result:=3;
     exit;
   end;
{Create Info}
  f.SearchSpec := Path;
  {We always also search for readonly and archive, regardless of Attr:}
  f.SearchAttr := Attr or archive or readonly;
  f.SearchPos  := 0;
  f.NamePos := Length(f.SearchSpec);
  while (f.NamePos>0) and not (f.SearchSpec[f.NamePos] in AllowDirectorySeparators) do
   dec(f.NamePos);
{Wildcards?}
  if (Pos('?',Path)=0)  and (Pos('*',Path)=0) then
   begin
     if FindGetFileInfo(Path,f) then
      result:=0
     else
      begin
        { According to tdos2 test it should return 18
        if ErrNo=Sys_ENOENT then
         result:=3
        else }
         result:=18;
      end;
     f.DirFD:=-1;
     f.SearchType:=1;
     f.searchnum:=-1;
   end
  else
{Find Entry}
   begin
     Inc(CurrSearchNum);
     f.SearchNum:=CurrSearchNum;
     f.SearchType:=0;
     result:=WasiFindNext(f);
   end;
End;


Function UniversalToEpoch(year,month,day,hour,minute,second:Word):int64;
const
  days_in_month: array [boolean, 1..12] of Byte =
    ((31,28,31,30,31,30,31,31,30,31,30,31),
     (31,29,31,30,31,30,31,31,30,31,30,31));
  days_before_month: array [boolean, 1..12] of Word =
    ((0,
      0+31,
      0+31+28,
      0+31+28+31,
      0+31+28+31+30,
      0+31+28+31+30+31,
      0+31+28+31+30+31+30,
      0+31+28+31+30+31+30+31,
      0+31+28+31+30+31+30+31+31,
      0+31+28+31+30+31+30+31+31+30,
      0+31+28+31+30+31+30+31+31+30+31,
      0+31+28+31+30+31+30+31+31+30+31+30),
     (0,
      0+31,
      0+31+29,
      0+31+29+31,
      0+31+29+31+30,
      0+31+29+31+30+31,
      0+31+29+31+30+31+30,
      0+31+29+31+30+31+30+31,
      0+31+29+31+30+31+30+31+31,
      0+31+29+31+30+31+30+31+31+30,
      0+31+29+31+30+31+30+31+31+30+31,
      0+31+29+31+30+31+30+31+31+30+31+30));
var
  leap: Boolean;
  days_in_year: LongInt;
  y,m: LongInt;
begin
  if (year<1970) or (month<1) or (month>12) or (day<1) or (day>31) or
     (hour>=24) or (minute>=60) or (second>=60) then
  begin
    result:=-1;
    exit;
  end;
  leap:=((year mod 4)=0) and (((year mod 100)<>0) or ((year mod 400)=0));
  if day>days_in_month[leap,month] then
  begin
    result:=-1;
    exit;
  end;
  result:=0;
  for y:=1970 to year-1 do
    if ((y mod 4)=0) and (((y mod 100)<>0) or ((y mod 400)=0)) then
      Inc(result,366)
    else
      Inc(result,365);
  Inc(result,days_before_month[leap,month]);
  Inc(result,day-1);
  result:=(((result*24+hour)*60+minute)*60)+second;
end;

Function LocalToEpoch(year,month,day,hour,minute,second:Word):int64;
begin
  { todo: convert UTC to local time, as soon as we can get the local timezone
    from WASI: https://github.com/WebAssembly/WASI/issues/239 }
  result:=UniversalToEpoch(year,month,day,hour,minute,second);
end;


Procedure EpochToUniversal(epoch:int64;var year,month,day,hour,minute,second:Word);
const
  days_in_month: array [boolean, 1..12] of Byte =
    ((31,28,31,30,31,30,31,31,30,31,30,31),
     (31,29,31,30,31,30,31,31,30,31,30,31));
var
  leap: Boolean;
  days_in_year: LongInt;
begin
  if epoch<0 then
  begin
    year:=0;
    month:=0;
    day:=0;
    hour:=0;
    minute:=0;
    second:=0;
    exit;
  end;
  second:=epoch mod 60;
  epoch:=epoch div 60;
  minute:=epoch mod 60;
  epoch:=epoch div 60;
  hour:=epoch mod 24;
  epoch:=epoch div 24;
  year:=1970;
  leap:=false;
  days_in_year:=365;
  while epoch>=days_in_year do
  begin
    Dec(epoch,days_in_year);
    Inc(year);
    leap:=((year mod 4)=0) and (((year mod 100)<>0) or ((year mod 400)=0));
    if leap then
      days_in_year:=366
    else
      days_in_year:=365;
  end;
  month:=1;
  Inc(epoch);
  while epoch>days_in_month[leap,month] do
  begin
    Dec(epoch,days_in_month[leap,month]);
    Inc(month);
  end;
  day:=Word(epoch);
end;


Procedure EpochToLocal(epoch:int64;var year,month,day,hour,minute,second:Word);
begin
  { todo: convert UTC to local time, as soon as we can get the local timezone
    from WASI: https://github.com/WebAssembly/WASI/issues/239 }
  EpochToUniversal(epoch,year,month,day,hour,minute,second);
end;

end.
