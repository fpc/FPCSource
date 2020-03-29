{$mode objfpc}
{$h+}

unit testu;

Interface

uses
  dos;
{ ---------------------------------------------------------------------
    utility functions, shared by several programs of the test suite
  ---------------------------------------------------------------------}

type
  TCharSet = set of char;

  TVerboseLevel=(V_Abort,V_Error,V_Warning,V_Normal,V_Debug,V_SQL);

  TConfig = record
    NeedOptions,
    DelOptions,
    NeedCPU,
    SkipCPU,
    SkipEmu,
    NeedTarget,
    SkipTarget,
    MinVersion,
    MaxVersion,
    KnownRunNote,
    KnownCompileNote,
    RecompileOpt: string;
    ResultCode    : longint;
    KnownRunError : longint;
    KnownCompileError : longint;
    NeedRecompile : boolean;
    NeedLibrary   : boolean;
    NeededAfter   : boolean;
    IsInteractive : boolean;
    IsKnownRunError,
    IsKnownCompileError : boolean;
    NoRun         : boolean;
    UsesGraph     : boolean;
    ShouldFail    : boolean;
    Timeout       : longint;
    Category      : string;
    Note          : string;
    Files         : string;
    ConfigFileSrc : string;
    ConfigFileDst : string;
    WpoParas      : string;
    WpoPasses     : longint;
    DelFiles      : string;
  end;

Const
  DoVerbose : boolean = false;
  DoSQL     : boolean = false;
  MaxLogSize : LongInt = 50000;


procedure TrimB(var s:string);
procedure TrimE(var s:string);
function upper(const s : string) : string;
procedure Verbose(lvl:TVerboseLevel;const s:string);
function GetConfig(const fn:string;var r:TConfig):boolean;
Function GetFileContents (FN : String) : String;

const
{ Constants used in IsAbsolute function }
  TargetHasDosStyleDirectories : boolean = false;
  TargetAmigaLike : boolean = false;
  TargetIsMacOS : boolean = false;
  TargetIsUnix : boolean = false;

{ File path helper functions }
function SplitPath(const s:string):string;
function SplitBasePath(const s:string): string;
Function SplitFileName(const s:string):string;
Function SplitFileBase(const s:string):string;
Function SplitFileExt(const s:string):string;
Function FileExists (Const F : String) : Boolean;
Function PathExists (Const F : String) : Boolean;
Function IsAbsolute (Const F : String) : boolean;
function GetToken(var s: string; Delims: TCharSet = [' ']):string;

Implementation

function GetToken(var s: string; Delims: TCharSet = [' ']):string;
var
  i : longint;
  p: PChar;
begin
  p:=PChar(s);
  i:=0;
  while (p^ <> #0) and not (p^ in Delims) do begin
    Inc(p);
    Inc(i);
  end;
  GetToken:=Copy(s,1,i);
  Delete(s,1,i+1);
end;

function SplitPath(const s:string):string;
var
  i : longint;
begin
  i:=Length(s);
  while (i>0) and not(s[i] in ['/','\'{$IFDEF MACOS},':'{$ENDIF}]) do
   dec(i);
  SplitPath:=Copy(s,1,i);
end;


function SplitBasePath(const s:string): string;
var
  i : longint;
begin
  i:=1;
  while (i<length(s)) and not(s[i] in ['/','\'{$IFDEF MACOS},':'{$ENDIF}]) do
   inc(i);
  if s[i] in  ['/','\'{$IFDEF MACOS},':'{$ENDIF}] then
    dec(i);
  SplitBasePath:=Copy(s,1,i);
end;

Function SplitFileName(const s:string):string;
var
  p : dirstr;
  n : namestr;
  e : extstr;
begin
  FSplit(s,p,n,e);
  SplitFileName:=n+e;
end;

Function SplitFileBase(const s:string):string;
var
  p : dirstr;
  n : namestr;
  e : extstr;
begin
  FSplit(s,p,n,e);
  SplitFileBase:=n;
end;

Function SplitFileExt(const s:string):string;
var
  p : dirstr;
  n : namestr;
  e : extstr;
begin
  FSplit(s,p,n,e);
  SplitFileExt:=e;
end;


Function FileExists (Const F : String) : Boolean;
{
  Returns True if the file exists, False if not.
}
Var
  info : searchrec;
begin
  FindFirst (F,anyfile,Info);
  FileExists:=DosError=0;
  FindClose (Info);
end;


Function PathExists (Const F : String) : Boolean;
{
  Returns True if the file exists, False if not.
}
Var
  info : searchrec;
begin
  FindFirst (F,anyfile,Info);
  PathExists:=(DosError=0) and (Info.Attr and Directory=Directory);
  FindClose (Info);
end;

{ extracted from rtl/macos/macutils.inc }

function IsMacFullPath (const path: string): Boolean;
  begin
    if Pos(':', path) = 0 then    {its partial}
      IsMacFullPath := false
    else if path[1] = ':' then
      IsMacFullPath := false
    else
      IsMacFullPath := true
  end;


Function IsAbsolute (Const F : String) : boolean;
{
  Returns True if the name F is a absolute file name
}
begin
  IsAbsolute:=false;
  if TargetHasDosStyleDirectories then
    begin
      if (F[1]='/') or (F[1]='\') then
        IsAbsolute:=true;
      if (Length(F)>2) and (F[2]=':') and ((F[3]='\') or (F[3]='/')) then
        IsAbsolute:=true;
    end
  else if TargetAmigaLike then
    begin
      if (length(F)>0) and (Pos(':',F) <> 0) then
        IsAbsolute:=true;
    end
  else if TargetIsMacOS then
    begin
      IsAbsolute:=IsMacFullPath(F);
    end
  { generic case }
  else if (F[1]='/') then
    IsAbsolute:=true;
end;

procedure Verbose(lvl:TVerboseLevel;const s:string);
begin
  case lvl of
    V_Normal :
      writeln(s);
    V_Debug :
      if DoVerbose then
       writeln('Debug: ',s);
    V_SQL :
      if DoSQL then
       writeln('SQL: ',s);
    V_Warning :
      writeln('Warning: ',s);
    V_Error :
      begin
        writeln('Error: ',s);
        halt(1);
      end;
    V_Abort :
      begin
        writeln('Abort: ',s);
        halt(0);
      end;
  end;
end;

procedure TrimB(var s:string);
begin
  while (s<>'') and (s[1] in [' ',#9]) do
   delete(s,1,1);
end;


procedure TrimE(var s:string);
begin
  while (s<>'') and (s[length(s)] in [' ',#9]) do
   delete(s,length(s),1);
end;


function upper(const s : string) : string;
var
  i,l  : longint;

begin
  L:=Length(S);
  SetLength(upper,l);
  for i:=1 to l do
    if s[i] in ['a'..'z'] then
     upper[i]:=char(byte(s[i])-32)
    else
     upper[i]:=s[i];
end;

function GetConfig(const fn:string;var r:TConfig):boolean;
var
  t : text;
  part,code : integer;
  l : longint;
  s,res : string;

  function GetEntry(const entry:string):boolean;
  var
    i : longint;
  begin
    Getentry:=false;
    Res:='';
    if Upper(Copy(s,1,length(entry)))=Upper(entry) then
     begin
       Delete(s,1,length(entry));
       TrimB(s);
       if (s<>'') then
        begin
          if (s[1]='=') then
           begin
             delete(s,1,1);
             i:=pos('}',s);
             if i=0 then
              i:=255
             else
              dec(i);
             res:=Copy(s,1,i);
             TrimB(res);
             TrimE(res);
           end;
          Verbose(V_Debug,'Config: '+Entry+' = "'+Res+'"');
          GetEntry:=true;
        end;
     end;
  end;

begin
  FillChar(r,sizeof(r),0);
  GetConfig:=false;
  Verbose(V_Debug,'Reading '+fn);
  assign(t,fn);
  {$I-}
   reset(t);
  {$I+}
  if ioresult<>0 then
   begin
     Verbose(V_Error,'Can''t open '+fn);
     exit;
   end;
  r.Note:='';
  while not eof(t) do
   begin
     readln(t,s);
     if Copy(s,1,3)=#$EF#$BB#$BF then
       delete(s,1,3);
     TrimB(s);
     if s<>'' then
      begin
        if s[1]='{' then
         begin
           delete(s,1,1);
           TrimB(s);
           if (s<>'') and (s[1]='%') then
            begin
              delete(s,1,1);
              if GetEntry('OPT') then
               r.NeedOptions:=res
              else
               if GetEntry('DELOPT') then
                r.DelOptions:=res
              else
               if GetEntry('TARGET') then
                r.NeedTarget:=res
              else
               if GetEntry('SKIPTARGET') then
                r.SkipTarget:=res
              else
               if GetEntry('CPU') then
                r.NeedCPU:=res
              else
               if GetEntry('SKIPCPU') then
                r.SkipCPU:=res
              else
               if GetEntry('SKIPEMU') then
                r.SkipEmu:=res
              else
               if GetEntry('VERSION') then
                r.MinVersion:=res
              else
               if GetEntry('MAXVERSION') then
                r.MaxVersion:=res
              else
               if GetEntry('RESULT') then
                Val(res,r.ResultCode,code)
              else
               if GetEntry('GRAPH') then
                r.UsesGraph:=true
              else
               if GetEntry('FAIL') then
                r.ShouldFail:=true
              else
               if GetEntry('RECOMPILE') then
	        begin
                  r.NeedRecompile:=true;
		  r.RecompileOpt:=res;
		end
              else
               if GetEntry('NORUN') then
                r.NoRun:=true
              else
               if GetEntry('NEEDLIBRARY') then
                r.NeedLibrary:=true
              else
               if GetEntry('NEEDEDAFTER') then
                r.NeededAfter:=true
              else
               if GetEntry('KNOWNRUNERROR') then
                begin
                  r.IsKnownRunError:=true;
                  if res<>'' then
                    begin
                      val(res,l,code);
                      if code>1 then
                        begin
                          part:=code;
                          val(copy(res,1,code-1),l,code);
                          delete(res,1,part);
                        end;
                      if code=0 then
                        r.KnownRunError:=l;
                      if res<>'' then
                        r.KnownRunNote:=res;
                    end;
                end
              else
               if GetEntry('KNOWNCOMPILEERROR') then
                begin
                  r.IsKnownCompileError:=true;
                  if res<>'' then
                    begin
                      val(res,l,code);
                      if code>1 then
                        begin
                          part:=code;
                          val(copy(res,1,code-1),l,code);
                          delete(res,1,part);
                        end;
                      if code=0 then
                        r.KnownCompileError:=l;
                      if res<>'' then
                        r.KnownCompileNote:=res;
                    end;
                end
              else
               if GetEntry('INTERACTIVE') then
                r.IsInteractive:=true
              else
               if GetEntry('NOTE') then
                begin
                  R.Note:='Note: '+res;
                  Verbose(V_Normal,r.Note);
                end
              else
               if GetEntry('TIMEOUT') then
                Val(res,r.Timeout,code)
              else
               if GetEntry('FILES') then
                r.Files:=res
              else
                if GetEntry('CONFIGFILE') then
                  begin
                    l:=Pos(' ',res);
                    if l>0 then
                      begin
                        r.ConfigFileSrc:=Copy(res,1,l-1);
                        r.ConfigFileDst:=Copy(res,l+1,Length(res)-l+1);
                        if r.ConfigFileSrc='' then
                          Verbose(V_Error,'Config file source is empty');
                        if r.ConfigFileDst='' then
                          Verbose(V_Error,'Config file destination is empty');
                      end
                    else
                      begin
                        r.ConfigFileSrc:=res;
                        r.ConfigFileDst:=res;
                      end;
                  end
              else
                if GetEntry('WPOPARAS') then
                 r.wpoparas:=res
              else
                if GetEntry('WPOPASSES') then
                 val(res,r.wpopasses,code)
              else
                if GetEntry('DELFILES') then
                  r.DelFiles:=res
              else
               Verbose(V_Error,'Unknown entry: '+s);
            end;
         end
        else
         break;
      end;
   end;
  close(t);
  GetConfig:=true;
end;

Function GetFileContents (FN : String) : String;

Var
  F : Text;
  S : String;

begin
  Result:='';
  Assign(F,FN);
  {$I-}
  Reset(F);
  If IOResult<>0 then
    Exit;
  {$I+}
  While Not(EOF(F)) do
    begin
    ReadLn(F,S);
    if length(Result)<MaxLogSize then
      Result:=Result+S+LineEnding;
    end;
  Close(F);
end;

end.
