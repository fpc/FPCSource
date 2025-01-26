{ ---------------------------------------------------------------------
    utility functions, shared by several programs of the test suite
  ---------------------------------------------------------------------}

{$mode objfpc}
{$modeswitch advancedrecords}
{$h+}

unit tsutils;

Interface

uses
  classes, sysutils, tstypes;

Type
  TOnVerboseEvent = procedure(lvl:TVerboseLevel; const aMsg : String) of object;

var
  OnVerbose : TOnVerboseEvent = Nil;
  IsCGI : boolean = false;
  DoVerbose : boolean = false;
  DoSQL     : boolean = false;
  MaxLogSize : LongInt = 50000;


procedure TrimB(var s:string);
procedure TrimE(var s:string);
function upper(const s : string) : string;
procedure Verbose(lvl:TVerboseLevel; const s:string);
function GetConfig(const logprefix,fn:string;out aConfig:TConfig):boolean;
function GetUnitTestConfig(const logprefix,fn,SrcDir: string; out aConfig : TConfig) : Boolean;
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

function posr(c : Char; const s : AnsiString) : integer;
var
  i : integer;
begin
  i := length(s);
  while (i>0) and (s[i] <> c) do dec(i);
  Result := i;
end;


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

begin
  Result:=ExtractFileName(S);
end;

Function SplitFileBase(const s:string):string;

begin
  Result:=ChangeFileExt(ExtractFileName(S),'');
end;

Function SplitFileExt(const s:string):string;
begin
  Result:=ExtractFileExt(S);
end;


Function FileExists (Const F : String) : Boolean;

begin
  Result:=SysUtils.FileExists(F);
end;


Function PathExists (Const F : String) : Boolean;
{
  Returns True if the file exists, False if not.
}

begin
  Result:=DirectoryExists(F);
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

const
  lPrefixes : Array[TVerboseLevel] of string = ('Abort','Error','Warning','Info','Debug','SQL');

var
  lOutput : String;

  Procedure DoOutput;

  begin
    if not IsCGI then
      begin
      Writeln(output,lOutput);
      Flush(output);
      end
    else
      begin
      Writeln(stderr,lOutput);
      Flush(stderr);
      end;
    if Assigned(OnVerbose) then
      OnVerbose(lvl,lOutput);
  end;

begin
  lOutput:=lPrefixes[lvl]+': '+S;
  case lvl of
    V_Normal :
      DoOutput;
    V_Debug :
      if DoVerbose then
        DoOutput;
    V_SQL :
      if DoSQL then
        DoOutput;
    V_Warning :
      DoOutput;
    V_Error :
      begin
        DoOutput;
        if not IsCGI then
          halt(1);
      end;
    V_Abort :
      begin
        DoOutput;
        if not IsCGI then
          halt(0);
      end;
  end;

end;

procedure TrimB(var s:string);
begin
  S:=TrimLeft(S);
end;


procedure TrimE(var s:string);
begin
  S:=TrimRight(S);
end;


function upper(const s : string) : string;
var
  i,l  : longint;

begin
  Result:='';
  L:=Length(S);
  SetLength(Result,l);
  for i:=1 to l do
    if s[i] in ['a'..'z'] then
     Result[i]:=char(byte(s[i])-32)
    else
     Result[i]:=s[i];
end;

function GetConfig(const logprefix,fn:string;out aConfig:TConfig):boolean;

  Procedure ExtractCodeAndNote(s : String; out aCode : Integer; out aNote : String);

  var
    i : Integer;

  begin
    aCode:=0;
    aNote:='';
    if S='' then
      exit;
    I:=1;
    While (i<=Length(s)) and (S[I] in ['0'..'9']) do
      Inc(i);
    if I>1 then
      aCode:=StrToIntDef(Copy(S,1,i-1),0);
    aNote:=Copy(S,I,Length(S)-I+1);
  end;

  function GetEntry(S : String; Out entry, Res :string):boolean;
  var
    i : longint;

  begin
    Result:=False;
    Entry:='';
    Res:='';
    S:=TrimLeft(s);
    if (s='') or (S[1]<>'{') then exit(False);
    Delete(S,1,1);
    S:=TrimLeft(s);
    if (s='') or (S[1]<>'%') then exit(False);
    Delete(S,1,1);
    S:=TrimLeft(s);
    i:=Pos('}',S);
    if I=0 then exit(False);
    S:=Copy(S,1,I-1);
    i:=Pos('=',S);
    if I=0 then
      Entry:=Trim(S)
    else
      begin
      Entry:=Trim(Copy(S,1,I-1));
      Res:=Trim(Copy(S,I+1,Length(S)-I));
      end;
    Result:=True;
    Verbose(V_Debug,'Config: '+Entry+' = "'+Res+'"');
  end;

  Procedure AnalyseEntry(aEntry,aValue : string);
  var
    l,p,code : Integer;

  begin
    case UpperCase(aEntry) of
      'OPT': aConfig.NeedOptions:=aValue;
      'DELOPT': aConfig.DelOptions:=aValue;
      'TARGET': aConfig.NeedTarget:=aValue;
      'SKIPTARGET': aConfig.SkipTarget:=aValue;
      'CPU': aConfig.NeedCPU:=aValue;
      'SKIPCPU': aConfig.SkipCPU:=aValue;
      'SKIPEMU': aConfig.SkipEmu:=aValue;
      'VERSION': aConfig.MinVersion:=aValue;
      'MAXVERSION': aConfig.MaxVersion:=aValue;
      'RESULT' : aConfig.ResultCode:=StrToIntDef(aValue,0);
      'GRAPH' : aConfig.UsesGraph:=true;
      'FAIL' : aConfig.ShouldFail:=true;
      'NORUN': aConfig.NoRun:=true;
      'NEEDLIBRARY': aConfig.NeedLibrary:=true;
      'NEEDEDAFTER': aConfig.NeededAfter:=true;
      'TIMEOUT': aConfig.Timeout:=StrToIntDef(aValue,0);
      'FILES': aConfig.Files:=aValue;
      'WPOPARAS': aConfig.wpoparas:=aValue;
      'WPOPASSES': aConfig.wpopasses:=StrToIntDef(aValue,0);
      'DELFILES': aConfig.DelFiles:=aValue;
      'INTERACTIVE': aConfig.IsInteractive:=true;
      'RECOMPILE':
         begin
         aConfig.NeedRecompile:=true;
         aConfig.RecompileOpt:=aValue;
         end;
      'KNOWNRUNERROR':
         begin
         aConfig.IsKnownRunError:=true;
         ExtractCodeAndNote(aValue,aConfig.KnownRunError,aConfig.KnownRunNote);
         end;
      'KNOWNCOMPILEERROR':
         begin
         aConfig.IsKnownCompileError:=true;
         ExtractCodeAndNote(aValue,aConfig.KnownCompileError,aConfig.KnownCompileNote);
         end;
      'NOTE':
         begin
         aConfig.Note:=aValue;
         Verbose(V_Normal,LogPrefix+aConfig.Note);
         end;
      'CONFIGFILE':
         begin
         l:=Pos(' ',aValue);
         if l>0 then
           begin
             aConfig.ConfigFileSrc:=Trim(Copy(aValue,1,l-1));
             aConfig.ConfigFileDst:=Trim(Copy(aValue,l+1,Length(aValue)-l+1));
             if aConfig.ConfigFileSrc='' then
               Verbose(V_Error,LogPrefix+' File '+fn+' Config file source is empty');
             if aConfig.ConfigFileDst='' then
               Verbose(V_Error,LogPrefix+' File '+fn+' Config file destination is empty');
           end
         else
           begin
             aConfig.ConfigFileSrc:=aValue;
             aConfig.ConfigFileDst:=aValue;
           end;
         end;
      'EXPECTMSGS':
         begin
           p:=Pos(',',aValue);
           while p>0 do
             begin
               val(Copy(aValue,1,p-1),l,code);
               if code<>0 then
                 Verbose(V_Error,LogPrefix+' File '+fn+' Invalid value in EXPECTMSGS list: '+Copy(aValue,1,p-1));
               Insert(l,aConfig.ExpectMsgs,Length(aConfig.ExpectMsgs));
               Delete(aValue,1,p);
               p:=Pos(',',aValue);
             end;
           Val(aValue,l,code);
           if code<>0 then
             Verbose(V_Error,LogPrefix+' File '+fn+' Invalid value in EXPECTMSGS list: '+aValue);
           Insert(l,aConfig.ExpectMsgs,Length(aConfig.ExpectMsgs));
         end;
       else
         Verbose(V_Error,LogPrefix+' File '+fn+' Unknown entry: '+aEntry+' with value: '+aValue);
    end;
  end;

var
  l : TStringList;
  lErr : longint;
  s,aEntry,aValue: string;

begin
  Result:=False;
  aConfig:=Default(TConfig);
  GetConfig:=false;
  Verbose(V_Debug,LogPrefix+'Reading '+fn);
  lErr:=0;
  L:=TStringList.Create;
  try
    try
      L.LoadFromFile(FN);
    except
      on E : Exception do
        begin
        Verbose(V_WARNING,'Error '+E.ClassName+' loading '+fn+': '+E.Message);
        exit;
        end;
    end;
    For S in L do
      begin
      if GetEntry(S,aEntry,aValue) then
        AnalyseEntry(aEntry,aValue)
      else
        Inc(lErr);
      if lErr>2 then
         Break;
      end;
  finally
    L.Free;
  end;
  Result:=true;
end;

Function GetFileContents (FN : String) : String;

begin
  Result:=Sysutils.GetFileAsString(FN);
end;

function GetUnitTestConfig(const logprefix,fn,SrcDir : string; out aConfig : TConfig) : Boolean;

var
  Path       : string;
  lClassName  : string;
  lMethodName : string;
  slashpos   : integer;
  FileName   : string;
  s,line     : string;
  Src : TStrings;

begin
  Result := False;
  aConfig:=Default(TConfig);
  if pos('.',fn) > 0 then exit; // This is normally not a unit-test
  slashpos := posr('/',fn);
  if slashpos < 1 then exit;
  lMethodName := copy(fn,slashpos+1,length(fn));
  Path := copy(fn,1,slashpos-1);
  slashpos := posr('/',Path);
  if slashpos > 0 then
    begin
    lClassName := copy(Path,slashpos+1,length(Path));
    Path := copy(Path,1,slashpos-1);
    end
  else
    begin
    lClassName := Path;
    path := '.';
    end;
  if upper(lClassName[1])<>'T' then exit;
  FileName := SrcDir+Path+DirectorySeparator+copy(lowercase(lClassName),2,length(lClassName));
  if FileExists(FileName+'.pas') then
    FileName := FileName + '.pas'
  else if FileExists(FileName+'.pp') then
    FileName := FileName + '.pp'
  else
    exit;
  Src:=TStringList.Create;
  try
    Verbose(V_Debug,logprefix+'Reading: '+FileName);
    Src.LoadFromFile(FileName);
    for Line in Src do
      if Line<>'' then
        begin
        s:=Line;
        TrimB(s);
        if SameText(copy(s,1,9),'PROCEDURE') then
          begin
           if pos(';',s)>11 then
            begin
              s := copy(s,11,pos(';',s)-11);
              TrimB(s);
              if SameText(s,lClassName+'.'+lMethodName) then
               begin
                 Result := True;
                 aConfig.Note:= 'unittest';
               end;
            end;
          end;
        end;
  finally
    Src.Free
  end;
end;



end.
