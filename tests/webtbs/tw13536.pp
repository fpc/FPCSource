
// without -O3 prints  0002020200010101
// with -O3 prints 0001010100010101

{$mode objfpc}{$H+}

uses sysutils;

Const
   MAXDWORD = $FFFFFFFF;


Type
 Filetime=longint;
 tchar=char;
 XWIN32_FIND_DATA = record
          dwFileAttributes : DWORD;
          ftLastWriteTime : FILETIME;
          nFileSizeHigh : DWORD;
          nFileSizeLow : DWORD;
          cFileName : array[0..(MAX_PATH)-1] of TCHAR;
       end;

Type
  TMySearchRec = Record
    Time : Longint;
    Size : Int64;
    Attr : Longint;
    Name : TFileName;
    ExcludeAttr : Longint;
    FindHandle : THandle;
    FindData : XWIN32_FIND_DATA;
  end;

function getlasterror:integer;
begin
end;

function aFindNextFile (F:thandle;fd:XWIN32_FIND_DATA):boolean;
begin

end;

function WinToDosTime( Var Wtime : FileTime;var DTime:longint):longbool;
begin
end;

Function aFindMatch(var f: TMySearchRec) : Longint;
begin
  // commenting code before the f.size:= line seems to alter the behaviour
  { Find file with correct attribute }
  While (F.FindData.dwFileAttributes and cardinal(F.ExcludeAttr))<>0 do
   begin
     if not aFindNextFile (F.FindHandle,F.FindData) then
      begin
        Result:=GetLastError;
        exit;
      end;
   end; 

  { Convert some attributes back }
  WinToDosTime(F.FindData.ftLastWriteTime,F.Time);

  f.size:=F.FindData.NFileSizeLow+(qword(maxdword)+1)*F.FindData.NFileSizeHigh;
  f.attr:=F.FindData.dwFileAttributes;
  f.Name:=StrPas(@F.FindData.cFileName[0]);
  Result:=0;
end;

var n : TMySearchRec;

begin 
 // make sure it gets past the while loop.
 n.finddata.dwfileattributes:=1;
 n.excludeattr:=0;

 n.FindData.NFileSizeLow:=$10101;
 n.FindData.NFileSizehigh:=$20202;
 // attempt to avoid problems with strpas.
 n.finddata.cfilename:='bla'#0;
 aFindMatch(n);

 writeln(n.size);
 writeln(inttohex(n.size,16));
 if (n.size<>$0002020200010101) then
   halt(1);
end.
