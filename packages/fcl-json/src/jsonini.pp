unit jsonini;
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canney and other members of the
    Free Pascal development team

    Json ini support

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}

interface

uses
  Classes, SysUtils, inifiles, fpjson, jsonscanner, jsonparser, dateutils;

type

  { TJSONIniFile }

  TJSONIniFile = class(TCustomIniFile)
  Private
    FJSON: TJSONObject;
    FCacheUpdates: Boolean;
    FDirty : Boolean;
    FStream: TStream;
    procedure SetCacheUpdates(const AValue: Boolean);
  protected
    Function GetRoot : TJSONObject;
    Function GetSection(Const ASectionName : String; AllowCreate : Boolean) : TJSONObject;
    Function GetKeyData(Const ASectionName,AKeyName : String) : TJSONData;
    // Return true if an existing item was replaced
    Function SetKeyData(Const ASectionName,AKeyName : String; AData : TJSONData) : Boolean;
    procedure MaybeUpdateFile;
    property Dirty : Boolean Read FDirty;
  public
    constructor Create(const AFileName: string; AOptions : TIniFileOptions = []); override; overload;
    constructor Create(AStream: TStream; AOptions : TJSONOptions); overload;
    destructor Destroy; override;
    Class Procedure ConvertIni(Const AIniFile,AJSONFile : String; StringsOnly : Boolean = True);
    function ReadString(const Section, Ident, Default: string): string; override;
    function ReadInteger(const Section, Ident: string; Default: Longint): Longint; override;
    function ReadInt64(const Section, Ident: string; Default: Int64): Int64; override;
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; override;
    function ReadDate(const Section, Ident: string; Default: TDateTime): TDateTime; override;
    function ReadDateTime(const Section, Ident: string; Default: TDateTime): TDateTime; override;
    function ReadFloat(const Section, Ident: string; Default: Double): Double; override;
    function ReadTime(const Section, Ident: string; Default: TDateTime): TDateTime; override;
    procedure WriteString(const Section, Ident, Value: String); override;
    procedure WriteDate(const Section, Ident: string; Value: TDateTime); override;
    procedure WriteDateTime(const Section, Ident: string; Value: TDateTime); override;
    procedure WriteFloat(const Section, Ident: string; Value: Double); override;
    procedure WriteTime(const Section, Ident: string; Value: TDateTime); override;
    procedure WriteInteger(const Section, Ident: string; Value: Longint); override;
    procedure WriteInt64(const Section, Ident: string; Value: Int64); override;
    procedure WriteBool(const Section, Ident: string; Value: Boolean); override;
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings; AOptions : TSectionValuesOptions = [svoIncludeInvalid]); overload; override;
    procedure EraseSection(const Section: string); override;
    procedure DeleteKey(const Section, Ident: String); override;
    procedure UpdateFile; override; overload;
    procedure UpdateFile(Const AFileName : string); overload;
    property Stream: TStream read FStream;
    property CacheUpdates : Boolean read FCacheUpdates write SetCacheUpdates;
  end;

implementation

{ TJSONIniFile }

procedure TJSONIniFile.SetCacheUpdates(const AValue: Boolean);
begin
  if FCacheUpdates and not AValue and FDirty then
    UpdateFile;
end;

function TJSONIniFile.GetRoot: TJSONObject;
begin
  Result:=FJSON;
end;

function TJSONIniFile.GetSection(const ASectionName: String; AllowCreate: Boolean): TJSONObject;

Var
  I : Integer;
  R : TJSONObject;

begin
  Result:=Nil;
  R:=GetRoot;
  I:=R.IndexOfName(ASectionName,True);
  if (I<>-1) and (R.Items[i].JSONType=jtObject) then
    Result:=R.Items[i] as TJSONObject
  else if AllowCreate then
    begin
    if (I<>-1) then
      R.Delete(I);
    Result:=TJSONObject.Create;
    R.Add(ASectionName,Result);
    end;
end;

function TJSONIniFile.GetKeyData(const ASectionName, AKeyName: String): TJSONData;

Var
  O : TJSONObject;
  I : integer;

begin
  Result:=Nil;
  O:=GetSection(ASectionName,False);
  if Assigned(O) then
    begin
    I:=O.IndexOfName(AKeyName,True);
    if (I<>-1) and (O.Items[i].JSONType in ActualValueJSONTypes) then
      Result:=O.Items[i];
    end
end;

function TJSONIniFile.SetKeyData(const ASectionName, AKeyName: String; AData: TJSONData): Boolean;
Var
  O : TJSONObject;
  I : integer;

begin
  O:=GetSection(ASectionName,true);
  I:=O.IndexOfName(AKeyName,True);
  Result:=(I<>-1);
  if Result then
    O.Delete(I);
  O.Add(aKeyName,AData);
  FDirty:=True;
end;

procedure TJSONIniFile.MaybeUpdateFile;
begin
  If FCacheUpdates then
    FDirty:=True
  else
    UpdateFile;
end;

constructor TJSONIniFile.Create(const AFileName: string; AOptions : TIniFileOptions = []);

Var
  F : TFileStream;

begin
  Inherited Create(AFileName,AOptions);
  if Not FileExists(AFileName) then
    FJSON:=TJSONObject.Create
  else
    begin
    F:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
    try
      Create(F,[joUTF8,joComments,joIgnoreTrailingComma]);
    finally
      F.Free;
    end;
    end;
end;

constructor TJSONIniFile.Create(AStream: TStream; AOptions: TJSONOptions);

Var
  P : TJSONParser;
  D : TJSONData;

begin
  D:=Nil;
  P:=TJSONParser.Create(AStream,AOptions);
  try
    D:=P.Parse;
    if (D is TJSONObject) then
      begin
      FJSON:=D as TJSONObject;
      D:=Nil;
      end
    else
      FJSON:=TJSONObject.Create;
  finally
    D.Free;
    P.Free;
  end;
end;

destructor TJSONIniFile.Destroy;
begin
  FreeAndNil(FJSON);
  inherited Destroy;
end;

class procedure TJSONIniFile.ConvertIni(const AIniFile, AJSONFile: String; StringsOnly: Boolean = true);

Var
  SIni : TMemIniFile;
  Dini : TJSONIniFile;
  S,K : TStrings;
  SN,KN,V : String;
  I6 : Int64;
  F : Double;
  B : Boolean;
  DT : TDateTime;

begin
  S:=Nil;
  K:=Nil;
  Dini:=Nil;
  SIni:=TMemIniFile.Create(AIniFile);
  try
    DIni:=Self.Create(AJSONFile);
    S:=TStringList.Create;
    K:=TStringList.Create;
    SIni.ReadSections(S);
    For SN in S do
      begin
      SIni.ReadSection(SN,K);
      For KN in K do
        begin
        V:=Sini.ReadString(SN,KN,'');
        if StringsOnly then
          Dini.WriteString(SN,KN,V)
        else
          begin
          If TryStrToInt64(V,I6) then
            Dini.WriteInt64(SN,KN,I6)
          else If TryStrToFloat(V,F) then
            Dini.WriteFloat(SN,KN,F)
          else If TryStrToBool(V,B) then
            Dini.WriteBool(SN,KN,B)
          else
            begin
            DT:=SIni.ReadTime(SN,KN,-1);
            B:=DT<>-1;
            if B then
              DIni.WriteTime(SN,KN,DT)
            else
              begin
              DT:=SIni.ReadDate(SN,KN,0);
              B:=DT<>0;
              if B then
                DIni.WriteDate(SN,KN,DT)
              else
                begin
                DT:=SIni.ReadDateTime(SN,KN,0);
                B:=DT<>0;
                if B then
                  DIni.WriteDateTime(SN,KN,DT)
                end;
              end;
            if Not B then
              Dini.WriteString(SN,KN,V)
            end;
          end;
        end;
      end;
    Dini.UpdateFile;
  finally
    FreeAndNil(S);
    FreeAndNil(K);
    FreeAndNil(Dini);
    FreeAndNil(Sini);
  end;
end;

function TJSONIniFile.ReadString(const Section, Ident, Default: string): string;

Var
  D : TJSONData;

begin
  D:=GetKeyData(Section,Ident);
  if Not Assigned(D) then
    Result:=Default
  else
    begin
    if D.JSONType in StructuredJSONTypes then
      Result:=D.AsJSON
    else
      Result:=D.AsString;
    end
end;

function TJSONIniFile.ReadInteger(const Section, Ident: string; Default: Longint): Longint;

Var
  D : TJSONData;
begin
  D:=GetKeyData(Section,Ident);
  if Not Assigned(D) then
    Result:=Default
  else
    if D.JSONType=jtNumber then
      Result:=D.AsInteger
    else
      if not TryStrToInt(D.AsString,Result) then
        Result:=Default;
end;

function TJSONIniFile.ReadInt64(const Section, Ident: string; Default: Int64): Int64;

Var
  D : TJSONData;

begin
  D:=GetKeyData(Section,Ident);
  if Not Assigned(D) then
    Result:=Default
  else
    if D.JSONType=jtNumber then
      Result:=D.AsInt64
    else
      if not TryStrToInt64(D.AsString,Result) then
        Result:=Default;
end;

function TJSONIniFile.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;

Var
  D : TJSONData;

begin
  D:=GetKeyData(Section,Ident);
  if Not Assigned(D) then
    Result:=Default
  else
    // Avoid exception frame
    if D.JSONType=jtBoolean then
      Result:=D.AsBoolean
    else
      try
        Result:=D.AsBoolean;
      except
        Result:=Default;
      end;
end;

function TJSONIniFile.ReadDate(const Section, Ident: string; Default: TDateTime): TDateTime;

Var
  D : TJSONData;

begin
  D:=GetKeyData(Section,Ident);
  if Not Assigned(D) then
    Result:=Default
  else if D.JSONType=jtNumber then
    Result:=TDateTime(D.AsFloat)
  else
    Result:=ScanDateTime('yyyy"-"mm"-"dd',D.AsString);
end;

function TJSONIniFile.ReadDateTime(const Section, Ident: string; Default: TDateTime): TDateTime;
Var
  D : TJSONData;

begin
  D:=GetKeyData(Section,Ident);
  if Not Assigned(D) then
    Result:=Default
  else if D.JSONType=jtNumber then
    Result:=TDateTime(D.AsFloat)
  else
    Result:=ScanDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss"."zzz',D.AsString);
end;

function TJSONIniFile.ReadFloat(const Section, Ident: string; Default: Double): Double;
Var
  D : TJSONData;
  C : Integer;

begin
  D:=GetKeyData(Section,Ident);
  if Not Assigned(D) then
    Result:=Default
  else
    if D.JSONType=jtNumber then
      Result:=D.AsFloat
    else
      // Localized
      if not TryStrToFloat(D.AsString,Result) then
        begin
        // Not localized
        Val(D.AsString,Result,C);
        if (C<>0) then
          Result:=Default;
        end;
end;

function TJSONIniFile.ReadTime(const Section, Ident: string; Default: TDateTime): TDateTime;

Var
  D : TJSONData;

begin
  D:=GetKeyData(Section,Ident);
  if Not Assigned(D) then
    Result:=Default
  else if D.JSONType=jtNumber then
    Result:=Frac(TDateTime(D.AsFloat))
  else
    Result:=ScanDateTime('"0000-00-00T"hh":"nn":"ss"."zzz',D.AsString);
end;

procedure TJSONIniFile.WriteString(const Section, Ident, Value: String);
begin
  SetKeyData(Section,Ident,CreateJSON(Value));
end;

procedure TJSONIniFile.WriteDate(const Section, Ident: string; Value: TDateTime);
begin
  SetKeyData(Section,Ident,CreateJSON(FormatDateTime('yyyy"-"mm"-"dd"T"00":"00":"00.zzz',Value)));
end;

procedure TJSONIniFile.WriteDateTime(const Section, Ident: string; Value: TDateTime);
begin
  SetKeyData(Section,Ident,CreateJSON(FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss.zzz',Value)));
end;

procedure TJSONIniFile.WriteFloat(const Section, Ident: string; Value: Double);
begin
  SetKeyData(Section,Ident,CreateJSON(Value));
end;

procedure TJSONIniFile.WriteTime(const Section, Ident: string; Value: TDateTime);
begin
  SetKeyData(Section,Ident,CreateJSON(FormatDateTime('0000"-"00"-"00"T"hh":"nn":"ss.zzz',Value)));
end;

procedure TJSONIniFile.WriteInteger(const Section, Ident: string; Value: Longint);
begin
  SetKeyData(Section,Ident,CreateJSON(Value));
end;

procedure TJSONIniFile.WriteInt64(const Section, Ident: string; Value: Int64);
begin
  SetKeyData(Section,Ident,CreateJSON(Value));
end;

procedure TJSONIniFile.WriteBool(const Section, Ident: string; Value: Boolean);
begin
  SetKeyData(Section,Ident,CreateJSON(Value));
end;

procedure TJSONIniFile.ReadSection(const Section: string; Strings: TStrings);
Var
  O : TJSONObject;
  E : TJSONEnum;

begin
  O:=GetSection(Section,False);
  if Assigned(O) then
    For E in O do
      If (E.Value.JSONType in ActualValueJSONTypes) then
        Strings.Add(E.Key);
end;

procedure TJSONIniFile.ReadSections(Strings: TStrings);

Var
  R : TJSONObject;
  E : TJSONEnum;

begin
  R:=GetRoot;
  for E in R do
    if E.Value.JSONType=jtObject then
      Strings.Add(E.Key);
end;

procedure TJSONIniFile.ReadSectionValues(const Section: string; Strings: TStrings; AOptions: TSectionValuesOptions);

Var
  O : TJSONObject;
  E : TJSONEnum;
  V : TJSONStringType;

begin
  O:=GetSection(Section,False);
  if Assigned(O) then
    For E in O do
      begin
      If (E.Value.JSONType in ActualValueJSONTypes) then
        begin
        V:=E.Value.AsString;
        Strings.Add(E.Key+'='+V);
        end
      else if (svoIncludeInvalid in AOptions) then
        begin
        V:=E.Value.AsJSON;
        Strings.Add(E.Key+'='+V);
        end
      end;
end;

procedure TJSONIniFile.EraseSection(const Section: string);

Var
  I : Integer;

begin
  I:=GetRoot.IndexOfName(Section,True);
  if (I<>-1) then
    begin
    GetRoot.Delete(I);
    MaybeUpdateFile;
    end;
end;

procedure TJSONIniFile.DeleteKey(const Section, Ident: String);

Var
  O : TJSONObject;
  I : integer;

begin
  O:=GetSection(Section,False);
  if O<>Nil then
    begin
    I:=O.IndexOfName(Ident,True);
    if I<>-1 then
      begin
      O.Delete(I);
      MaybeUpdateFile;
      end;
    end;
end;

procedure TJSONIniFile.UpdateFile;


begin
  If (FileName<>'') then
    UpdateFile(FileName)
end;

procedure TJSONIniFile.UpdateFile(const AFileName: string);

Var
  S : TJSONStringType;

begin
  With TFileStream.Create(AFileName,fmCreate) do
    try
      S:=FJSON.FormatJSON();
      WriteBuffer(S[1],Length(S));
    finally
      Free;
    end;
end;

end.

