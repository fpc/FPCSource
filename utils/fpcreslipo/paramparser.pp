{

    FPCResLipo - Free Pascal External Resource Thinner
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
  Classes, SysUtils, externaltypes;

type
  EParametersException = class(Exception);
  EOutputFileAlreadySetException = class(EParametersException);
  EUnknownParameterException = class(EParametersException);
  EArgumentMissingException = class(EParametersException);
  EUnknownEndianessException = class(EParametersException);

type

  { TParameters }

  TParameters = class
  private
    fHelp : boolean;
    fVersion : boolean;
    fVerbose : boolean;
    fInputFiles : TStringList;
    fOutputFile : string;
    fEndianess : byte;

    procedure ParseOutputFile(aList : TStringList; var index : integer; const parname : string);
    procedure ParseEndianess(aList : TStringList; var index : integer; const parname : string);
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
    property Endianess : byte read fEndianess write fEndianess;
  end;

implementation

uses
  msghandler;

{ TParameters }

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

procedure TParameters.ParseEndianess(aList: TStringList; var index: integer;
  const parname: string);
var tmp : string;
begin
  inc(index);
  tmp:=LowerCase(DoMandatoryArgument(aList,index));
  if tmp='' then
    raise EArgumentMissingException.Create(parname);

  if tmp='big' then fEndianess:=EXT_ENDIAN_BIG
  else if tmp='little' then fEndianess:=EXT_ENDIAN_LITTLE
  else raise EUnknownEndianessException.Create(tmp);
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
        else if ((tmp='-o') or (tmp='--output')) then
          ParseOutputFile(fList,i,tmp)
        else if ((tmp='-e') or (tmp='--endian')) then
          ParseEndianess(fList,i,tmp)
        else
          raise EUnknownParameterException.Create(tmp);
      end
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
  fEndianess:=EXT_ENDIAN_BIG;
end;

destructor TParameters.Destroy;
begin
  fInputFiles.Free;
end;

end.
