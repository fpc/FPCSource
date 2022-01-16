{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Resource reader/compiler for MS RC script files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit rcreader;

{$MODE OBJFPC} {$H+}

interface

uses
  Classes, SysUtils, resource;

type

  { TRCResourceReader }

  TRCResourceReader = class(TAbstractResourceReader)
  private
    fExtensions : string;
    fDescription : string;
    fRCIncludeDirs: TStringList;
    fRCDefines: TStringList;
  protected
    function GetExtensions : string; override;
    function GetDescription : string; override;
    procedure Load(aResources : TResources; aStream : TStream); override;
    function CheckMagic(aStream : TStream) : boolean; override;
    procedure ReadRCFile(aResources : TResources; aLocation: String; aStream : TStream);
  public
    constructor Create; override;
    destructor Destroy; override;
    property RCIncludeDirs: TStringList read fRCIncludeDirs;
    property RCDefines: TStringList read fRCDefines;
  end;

implementation

uses
  StreamIO, resdatastream, resfactory, lexlib, rcparser;

{ TRCResourceReader }

function TRCResourceReader.GetExtensions: string;
begin
  Result:=fExtensions;
end;

function TRCResourceReader.GetDescription: string;
begin
  Result:=fDescription;
end;

procedure TRCResourceReader.Load(aResources: TResources; aStream: TStream);
var
  fd: String;
begin
  if aStream is TFileStream then
    fd:= ExtractFilePath(TFileStream(aStream).FileName)
  else
    fd:= IncludeTrailingPathDelimiter(GetCurrentDir);
  try
    ReadRCFile(aResources, fd, aStream);
  except
    on e : EReadError do
      raise EResourceReaderUnexpectedEndOfStreamException.Create('');
  end;
end;

function TRCResourceReader.CheckMagic(aStream: TStream): boolean;
begin
  { TODO : Check for Text-Only file }
  Result:= True;
end;

procedure TRCResourceReader.ReadRCFile(aResources: TResources; aLocation: String; aStream: TStream);
var
  i: Integer;
begin
  AssignStream(lexlib.yyinput, aStream);
  Reset(lexlib.yyinput);
  try
    rcparser.yyfilename:= '#MAIN.RC';
    rcparser.SetDefaults;
    SetTextCodePage(lexlib.yyinput, rcparser.opt_code_page);
    rcparser.yinclude:= tyinclude.Create;
    rcparser.yinclude.WorkDir:= aLocation;
    rcparser.yinclude.SearchPaths.Assign(fRCIncludeDirs);
    rcparser.ypreproc:= typreproc.Create;
    rcparser.ypreproc.Defines.Add('RC_INVOKED', '');
    for i:= 0 to fRCDefines.Count-1 do
      rcparser.ypreproc.Defines.KeyData[fRCDefines.Names[i]]:= fRCDefines.ValueFromIndex[i];
    rcparser.aktresources:= aResources;
    if rcparser.yyparse <> 0 then
      raise EReadError.Create('Parse Error');
  finally
    rcparser.DisposePools;
    FreeAndNil(rcparser.ypreproc);
    FreeAndNil(rcparser.yinclude);
  end;
end;

constructor TRCResourceReader.Create;
begin
  fExtensions:='.rc';
  fDescription:='RC script resource reader';
  fRCDefines:= TStringList.Create;
  fRCIncludeDirs:= TStringList.Create;
end;

destructor TRCResourceReader.Destroy;
begin
  fRCIncludeDirs.Free;
  fRCDefines.Free;
  inherited;
end;

initialization
  { don't register automatically, as this is essentially a "catch all" }
  //TResources.RegisterReader('.rc',TRCResourceReader);

end.
