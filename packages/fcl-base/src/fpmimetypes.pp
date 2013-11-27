{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by the Free Pascal development team

    Mime Types Lookup/Management class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpmimetypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

Type

  { TMimeType }

  TMimeType = Class(TObject)
  private
    FExtensions: String;
    FExtentions: String;
    FMimeType: String;
  Public
    Constructor Create(Const AMimeType,AExtensions : String);
    Procedure MergeExtensions(AExtensions : String);
    Property MimeType : String Read FMimeType Write FMimeType;
    Property Extensions : String Read FExtensions Write FExtentions;
  end;

  { TFPMimeTypes }

  TFPMimeTypes = Class(TComponent)
  Private
    FTypes : TFPHashList;
    FExtensions : TFPHashList;
    procedure ParseLine(ALine: String; out Mime, Extensions: String);
  Protected
    Function FindMimeByType(Const AMime : String) : TMimeType;
    Function FindMimeByExt(Const AExt : String) : TMimeType;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    // Extract an extension from an extension list as returned by GetMimeExtensions
    class function GetNextExtension(var E: String): string;
    // Load from stream
    procedure LoadFromStream(Const Stream : TStream); virtual;
    // Load from file
    procedure LoadFromFile(Const AFileName : string);
    // Add one type to the list. AMimeType is converted to lowercase,
    // AExtensions is a semicolon separated list of extensions. (no dot)
    Procedure AddType(Const AMimeType,AExtensions : String);
    // Get known extensions for a Mime Type. Empty if unknown. Case insensitive.
    Function GetMimeExtensions(Const AMimeType : String) : String;
    // Get mime type for an extension. Empty if unknown extension. Initial dot is stripped.
    Function GetMimeType(Const AExtension : String) : String;
    // Fill AList with known mime types. No particular order.
    Function GetKnownMimeTypes(AList : TStrings) : Integer;
    // Fill AList with known extensions types. No particular order.
    Function GetKnownExtensions(AList : TStrings) : Integer;
  end;

Function MimeTypes : TFPMimeTypes;

implementation

{ TFPMimeTypes }
var
  FTypes : TFPMimeTypes;

Class Function TFPMimeTypes.GetNextExtension(var E : String) : string;

Var
  P : Integer;
begin
  P:=Pos(';',E);
  If (P=0) then P:=Length(E)+1;
  Result:=Copy(E,1,P-1);
  Delete(E,1,P);
end;

Function MimeTypes : TFPMimeTypes;

begin
  If (FTypes=Nil) then
    FTypes:=TFPMimeTypes.Create(Nil);
  Result:=FTypes;
end;

Procedure TFPMimeTypes.ParseLine(ALine : String; Out Mime,Extensions : String);

COnst
  WhiteSpace = [' ',#9];

  Function GetNextWord(S : String; Var APos : Integer) : String;
  Var
    SPos : Integer;
  begin
    While (APos<=Length(S)) and (S[APos] in Whitespace) do
      Inc(APos);
    SPos:=APos;
    While (APos<=Length(S)) and not (S[APos] in Whitespace) do
      Inc(APos);
    Result:=Copy(S,SPos,APos-SPos);
  end;

Var
  P : Integer;
  S : String;

begin
  P:=1;
  Mime:=GetNextWord(ALine,p);
  Repeat
    S:=GetNextWord(ALine,P);
    if (length(S)>0) and (S[1]='.') then
      Delete(S,1,1);
    If (S<>'') then
      Extensions:=Extensions+S+';';// always add ;
  until (S='');
end;

function TFPMimeTypes.FindMimeByType(const AMime: String): TMimeType;

Var
  I : integer;

begin
  I:=FTypes.FindIndexOf(LowerCase(AMime));
  If (I<>-1) then
    Result:=TMimeType(FTypes.Items[I])
  else
    Result:=Nil;
end;

function TFPMimeTypes.FindMimeByExt(const AExt: String): TMimeType;
Var
  I : integer;
  E : String;
begin
  if Length(AExt) = 0 then 
    Result:=Nil
  else 
    begin
    E:=LowerCase(AExt);
    If (E[1]='.') then
      Delete(E,1,1);
    I:=FExtensions.FindIndexOf(E);
    If (I<>-1) then
      Result:=TMimeType(FExtensions.Items[I])
    else
      Result:=Nil;
    end
end;

constructor TFPMimeTypes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTypes:=TFPHashList.Create;
  FExtensions:=TFPHashList.Create;
end;

destructor TFPMimeTypes.Destroy;

Var
  T : TMimeType;
  I : integer;

begin
  For I:=FTypes.Count-1 downto 0 do
    begin
    T:=TMimeType(FTypes.Items[i]);
    FreeAndNil(T);
    end;
  FreeAndNil(FTypes);
  FreeAndNil(FExtensions);
  inherited Destroy;
end;

procedure TFPMimeTypes.LoadFromStream(const Stream: TStream);

Var
  L : TStringList;
  S,M,E : String;
  I : Integer;

begin
  L:=TStringList.Create;
  try
    L.LoadFromStream(Stream);
    For I:=0 to L.Count-1 do
      begin
      S:=Trim(L[I]);
      If (S<>'') and (S[1]<>'#') then
        begin
        ParseLine(S,M,E);
        If (M<>'') then
          AddType(M,E);
        end;
      end;
  finally
    L.Free;
  end;
end;

procedure TFPMimeTypes.LoadFromFile(const AFileName: string);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(AFileName,fmOpenRead);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TFPMimeTypes.AddType(const AMimeType, AExtensions: String);

Var
  M,E,N : String;
  MT : TMimeType;
  I : Integer;

begin
  M:=LowerCase(AMimeType);
  E:=LowerCase(AExtensions);
  I:=FTypes.FindINdexOf(AMimeType);
  if (i=-1) then
    begin
    MT:=TMimeType.Create(M,E);
    FTypes.Add(M,MT);
    end
  else
    begin
    MT:=TMimeType(FTypes.Items[i]);
    MT.MergeExtensions(AExtensions);
    end;
  repeat
    N:=GetNextExtension(E);
    If (N<>'') then
      begin
      I:=FExtensions.FindIndexOf(N);
      If (I=-1) then
        FExtensions.Add(N,MT);
      end;
  until (n='');
end;

function TFPMimeTypes.GetMimeExtensions(const AMimeType: String): String;

Var
  T : TMimeType;

begin
  T:=FindMimeByType(AMimeType);
  if Assigned(T) then
    Result:=T.Extensions
  else
    Result:='';
end;

function TFPMimeTypes.GetMimeType(const AExtension: String): String;
Var
  T : TMimeType;

begin
  T:=FindMimeByExt(AExtension);
  if Assigned(T) then
    Result:=T.MimeType
  else
    Result:='';
end;

function TFPMimeTypes.GetKnownMimeTypes(AList: TStrings): Integer;

var
  i : Integer;

begin
  AList.BeginUpdate;
  try
    AList.Clear;
    For I:=0 to FTypes.Count-1 do
      Alist.Add(FTypes.NameOfIndex(i));
  finally
    AList.EndUpdate;
  end;
end;

function TFPMimeTypes.GetKnownExtensions(AList: TStrings): Integer;
var
  i : Integer;

begin
  AList.BeginUpdate;
  try
    AList.Clear;
    For I:=0 to FExtensions.Count-1 do
      Alist.Add(FExtensions.NameOfIndex(i));
  finally
    AList.EndUpdate;
  end;
end;

{ TMimeType }

constructor TMimeType.Create(const AMimeType, AExtensions: String);
begin
  FMimeType:=Lowercase(AMimeType);
  FExtensions:=Lowercase(AExtensions);
end;


procedure TMimeType.MergeExtensions(AExtensions: String);


var
  E : String;

begin
  Repeat
    E:=TFPMimeTypes.GetNextExtension(AExtensions);
    If (E<>'') then
      begin
      E:=E+';';
      If (Copy(Fextensions,1,Length(E))<>E) and (Pos(E,FExtensions)=0) then
        FExtensions:=Extensions+E;
      end;
  Until (E='')
end;

initialization

finalization
  FreeAndNil(FTypes);
end.

