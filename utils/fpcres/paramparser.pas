{

    FPCRes - Free Pascal Resource Converter
    Part of the Free Pascal distribution
    Copyright (C) 2008 by Giulio Bernardi
    
    Handles the parsing of parameters

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit paramparser;

{$MODE OBJFPC} {$H+}

interface

uses
  Classes, SysUtils, target;

type
  EParametersException = class(Exception);
  EOutputFileAlreadySetException = class(EParametersException);
  EUnknownParameterException = class(EParametersException);
  EArgumentMissingException = class(EParametersException);
  EUnknownObjFormatException = class(EParametersException);
  EUnknownMachineException = class(EParametersException);
  ECannotReadConfFile = class(EParametersException);

type

  { TParameters }

  TParameters = class
  private
    fHelp : boolean;
    fVersion : boolean;
    fVerbose : boolean;
    fInputFiles : TStringList;
    fOutputFile : string;
    fTarget : TResTarget;

    procedure ParseInputFiles(aList : TStringList; var index : integer; const parname : string);
    procedure ParseOutputFile(aList : TStringList; var index : integer; const parname : string);
    procedure ParseOutputFormat(aList : TStringList; var index : integer; const parname : string);
    procedure ParseArchitecture(aList : TStringList; var index : integer; const parname : string);
    procedure ParseConfigFile(aList : TStringList; var index : integer; const parname : string);
    function DoOptionalArgument(aList : TStringList; const i : integer) : string;
    function DoMandatoryArgument(aList : TStringList; const i : integer) : string;
    function IsParameter(const s : string) : boolean;
    function ParamsToStrList : TStringList;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse;
    property Help : boolean read fHelp;
    property Version : boolean read fVersion;
    property Verbose : boolean read fVerbose;
    property InputFiles : TStringList read fInputFiles;
    property OutputFile : string read fOutputFile write fOutputFile;
    property Target : TResTarget read fTarget;
  end;

implementation

uses
  msghandler;

type

  { TConfFileParser }

  TConfFileParser = class
  private
    fConfFile : TStringList;
    fParList : TStringList;
    fInsPos : integer;

    procedure ParseLine(idx : integer);
    function GetParameter(pc : pchar; var i : integer) : string;
    function GetString(pc : pchar; var i : integer) : string;
  protected
  public
    constructor Create(aFileName : string; aParList : TStringList; aInsPos : integer);
    procedure Parse;
    destructor Destroy; override;
  end;

{ TConfFileParser }

procedure TConfFileParser.ParseLine(idx: integer);
var pc : pchar;
    tmp : string;
    i : integer;
begin
  pc:=pchar(fConfFile[idx]);
  i:=0;
  while pc[i]<>#0 do
  begin
    case pc[i] of
      ' ',#9,#13,#10 : inc(i);
      '#' : break
    else
      begin
        tmp:=GetParameter(pc,i);
        if tmp<>'' then
        begin
          fParList.Insert(fInsPos,tmp);
          inc(fInsPos);
        end;
      end;
    end;
  end;
end;

function TConfFileParser.GetParameter(pc : pchar; var i : integer): string;
begin
  Result:='';
  while pc[i]<>#0 do
  begin
    case pc[i] of
      ' ',#9,#13,#10 : exit;
      '#' : exit;
      '"' : Result:=Result+GetString(pc,i);
      else
        Result:=Result+pc[i];
    end;
    inc(i);
  end;
end;

function TConfFileParser.GetString(pc: pchar; var i: integer): string;
begin
  Result:='';
  inc(i);
  while pc[i]<>#0 do
  begin
    if pc[i] = '"' then
      exit
    else
      Result:=Result+pc[i];
    inc(i);
  end;
  dec(i);
end;

constructor TConfFileParser.Create(aFileName: string; aParList: TStringList; aInsPos : integer);
begin
  fInsPos:=aInsPos+1;
  fConfFile:=TStringList.Create;
  fParList:=aParList;
  try
    fConfFile.LoadFromFile(aFileName);
  except
    raise ECannotReadConfFile.Create(aFileName);
  end;
end;

procedure TConfFileParser.Parse;
var i : integer;
begin
  for i:=0 to fConfFile.Count-1 do
    ParseLine(i);
end;

destructor TConfFileParser.Destroy;
begin
  fConfFile.Free;
end;

{ TParameters }

//for compatibility allow -i <inputfiles>
procedure TParameters.ParseInputFiles(aList: TStringList; var index: integer;
  const parname : string);
var tmp : string;
begin
  tmp:=DoMandatoryArgument(aList,index+1);
  if tmp='' then
    raise EArgumentMissingException.Create(parname);

  while tmp<>'' do
  begin
    inc(index);
    fInputFiles.Add(tmp);
    tmp:=DoOptionalArgument(aList,index+1);
  end;
end;

procedure TParameters.ParseOutputFile(aList: TStringList; var index: integer;
  const parname : string);
begin
  if fOutputFile<>'' then
    raise EOutputFileAlreadySetException.Create('');
  inc(index);
  fOutputFile:=DoMandatoryArgument(aList,index);
  if fOutputFile='' then
    raise EArgumentMissingException.Create(parname);
end;

procedure TParameters.ParseOutputFormat(aList: TStringList; var index: integer;
  const parname: string);
var tmp : string;
    aFormat : TObjFormat;
begin
  inc(index);
  tmp:=DoMandatoryArgument(aList,index);
  if tmp='' then
    raise EArgumentMissingException.Create(parname);

  for aFormat:=low(TObjFormat) to high(TObjFormat) do
  begin
    if ObjFormats[aFormat].name=tmp then
    begin
      fTarget.objformat:=aFormat;
      exit;
    end;
  end;
  
  raise EUnknownObjFormatException.Create(tmp);

end;

procedure TParameters.ParseArchitecture(aList: TStringList; var index: integer;
  const parname: string);
var tmp : string;
    aMachine : TMachineType;
begin
  inc(index);
  tmp:=DoMandatoryArgument(aList,index);
  if tmp='' then
    raise EArgumentMissingException.Create(parname);

  for aMachine:=low(TMachineType) to high(TMachineType) do
  begin
    if Machines[aMachine].name=tmp then
    begin
      fTarget.machine:=aMachine;
      exit;
    end;
  end;

  raise EUnknownMachineException.Create(tmp);

end;

procedure TParameters.ParseConfigFile(aList: TStringList; var index: integer;
  const parname : string);
var tmp : string;
    cp : TConfFileParser;
begin
  tmp:=copy(parname,2,length(parname)-1);
  if tmp='' then
    raise EArgumentMissingException.Create(parname);
  cp:=TConfFileParser.Create(tmp,aList,index);
  try
    cp.Parse;
  finally
    cp.Free;
  end;
end;

function TParameters.DoOptionalArgument(aList: TStringList; const i: integer
  ): string;
begin
  Result:='';
  if aList.Count>i then
  begin
    if not IsParameter(aList[i]) then
      Result:=aList[i];
  end;
end;

function TParameters.DoMandatoryArgument(aList: TStringList; const i: integer
  ): string;
begin
  Result:='';
  if aList.count>i then
    Result:=aList[i];
end;

function TParameters.IsParameter(const s: string): boolean;
begin
  Result:=false;
  if length(s)<=1 then exit;
  if copy(s,1,1)='-' then Result:=true;
end;

function TParameters.ParamsToStrList: TStringList;
var i : integer;
begin
  Result:=TStringList.Create;
  try
    for i:=1 to ParamCount do
      Result.Add(ParamStr(i));
  except
    Result.Free;
    raise;
  end;
end;

procedure TParameters.Parse;
var fList : TStringList;
    tmp : string;
    i : integer;
begin
  fList:=ParamsToStrList;
  try
    i:=0;
    while i<fList.Count do
    begin
      tmp:=fList[i];
      Messages.DoVerbose(Format('parsing parameter ''%s''',[tmp]));
      if IsParameter(tmp) then
      begin
        if ((tmp='--help') or (tmp='-h') or (tmp='-?')) then
          fHelp:=true
        else if ((tmp='--version') or (tmp='-V')) then
          fVersion:=true
        else if ((tmp='--verbose') or (tmp='-v')) then
          fVerbose:=true
        else if ((tmp='-i') or (tmp='--input')) then
          ParseInputFiles(fList,i,tmp)
        else if ((tmp='-o') or (tmp='--output')) then
          ParseOutputFile(fList,i,tmp)
        else if (tmp='-of') then
          ParseOutputFormat(fList,i,tmp)
        else if ((tmp='-a') or (tmp='--arch')) then
          ParseArchitecture(fList,i,tmp)
        else
          raise EUnknownParameterException.Create(tmp);
      end
      else
        if copy(tmp,1,1)='@' then
          ParseConfigFile(fList,i,tmp)
      else
        fInputFiles.Add(tmp); //assume it is an input file
      inc(i);
    end;
  finally
    fList.Free;
  end;
end;

constructor TParameters.Create;
begin
  fHelp:=false;
  fVersion:=false;
  fVerbose:=false;
  fInputFiles:=TStringList.Create;
  fOutputFile:='';
  fTarget.machine:=mtnone;
  fTarget.objformat:=ofnone;
end;

destructor TParameters.Destroy;
begin
  fInputFiles.Free;
end;

end.
