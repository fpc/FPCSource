unit extjsdataset;
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canney and other members of the
    Free Pascal development team

    extjs dataset

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, fpjson, typinfo, fpjsondataset;

Type
   { TExtJSJSONDataSet }

  // Base for ExtJS datasets. It handles MetaData conversion.
  TExtJSJSONDataSet = Class(TBaseJSONDataset)
  Private
    FFields : TJSONArray;
  Protected
    Function GenerateMetaData : TJSONObject;
    function ConvertDateFormat(const S: String): String; virtual;
    Procedure MetaDataToFieldDefs; override;
    procedure InitDateTimeFields; override;
    function StringToFieldType(const S: String): TFieldType;virtual;
    function GetStringFieldLength(F: TJSONObject; const AName: String; AIndex: Integer): integer; virtual;
  Public
    // Use this to load MetaData/Rows from stream.
    // If no metadata is present in the stream, FieldDefs must be filled manually.
    Procedure LoadFromStream(S : TStream);
    // Use this to load MetaData/Rows from file.
    // If no metadata is present in the file, FieldDefs must be filled manually.
    Procedure LoadFromFile(Const AFileName: string);
    // Use this to save Rows and optionally metadata to Stream.
    // Note that MetaData must be set.
    Procedure SaveToStream(S : TStream; SaveMetaData : Boolean);
    // Use this to save Rows and optionally metadata to Stream.
    // Note that MetaData must be set.
    Procedure SaveToFile(Const AFileName : String; SaveMetaData : Boolean);
    // Can be set directly if the dataset is closed.
    Property MetaData;
    // Can be set directly if the dataset is closed. If metadata is set, it must match the data.
    Property Rows;
  Published
    Property OwnsData;
  end;

  { TExtJSJSONObjectDataSet }
  // Use this dataset for data where the data is an array of objects.
  TExtJSJSONObjectDataSet = Class(TExtJSJSONDataSet)
    Function CreateFieldMapper : TJSONFieldMapper; override;
  end;

  { TExtJSJSONArrayDataSet }
  // Use this dataset for data where the data is an array of arrays.
  TExtJSJSONArrayDataSet = Class(TExtJSJSONDataSet)
    Function CreateFieldMapper : TJSONFieldMapper; override;
  end;


implementation

{ TExtJSJSONDataSet }


Function  TExtJSJSONDataSet.StringToFieldType(const S : String) : TFieldType;

begin
  if (s='int') then
    Result:=ftLargeInt
  else if (s='float') then
    Result:=ftFloat
  else if (s='boolean') then
    Result:=ftBoolean
  else if (s='date') then
    Result:=ftDateTime
  else if (s='string') or (s='auto') or (s='') then
    Result:=ftString
  else
    if MapUnknownToStringType then
      Result:=ftString
    else
      Raise EJSONDataset.CreateFmt('Unknown JSON data type : %s',[s]);
end;

Function  TExtJSJSONDataSet.GetStringFieldLength(F : TJSONObject; const AName : String; AIndex : Integer) : integer;

Var
  I,L : Integer;
  D : TJSONData;

begin
  Result:=0;
  I:=F.IndexOfName('maxlen');
  if (I<>-1) and (F.Items[I].jsonType=jtNumber) then
    begin
    Result:=StrToIntDef(trim(F.Items[i].AsString),-1);
    if (Result=-1) then
      Raise EJSONDataset.CreateFmt('Invalid maximum length specifier for field %s : %s',[AName,F.Items[i].AsString])
    end
  else
    begin
    For I:=0 to Rows.Count-1 do
      begin
      D:=FieldMapper.GetJSONDataForField(Aname,AIndex,Rows[i]);
      if (D<>Nil) and (D.JsonType<>jtNull) then
        begin
        l:=Length(D.AsString);
        if L>Result then
          Result:=L;
        end;
      end;
    end;
  if (Result=0) then
    Result:=20;
end;

procedure TExtJSJSONDataSet.LoadFromStream(S: TStream);

Var
  D : TJSONData;
  O : TJSONObject;
  N : String;
  I : Integer;

begin
  D:=GetJSON(S);
  try
    if (D.JSONType=jtObject) then
      O:=D as TJSONObject
    else
      begin
      FreeAndNil(D);
      Raise EJSONDataset.Create('Not a valid ExtJS JSON data packet');
      end;
    N:='rows';
    // Check metadata
    I:=O.IndexOfName('metaData');
    if (I<>-1) then
      begin
      If (O.Items[i].JSONType<>jtObject) then
        Raise EJSONDataset.Create('Invalid ExtJS JSON metaData in data packet.');
      Metadata:=O.Objects['metaData'];
      O.Extract(I);
      I:=Metadata.IndexOfName('root');
      If (I<>-1) then
        begin
        if (MetaData.Items[i].JSONType<>jtString) then
          Raise EJSONDataset.Create('Invalid ExtJS JSON root element in metaData.');
        N:=MetaData.Strings['root'];
        end;
      end;
    // Check rows
    I:=O.IndexOfName(N);
    if (I=-1) then
      Raise EJSONDataset.Create('Missing rows in data packet');
    if (O.Items[i].JSONType<>jtArray) then
      Raise EJSONDataset.Create('Rows element must be an array');
    Rows:=O.Items[i] as TJSONArray;
    O.Extract(I);
    OwnsData:=True;
  finally
    D.Free;
  end;
end;

procedure TExtJSJSONDataSet.LoadFromFile(const AFileName: string);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(F);
  finally
    F.Free;
  end;
end;

procedure TExtJSJSONDataSet.SaveToStream(S: TStream; SaveMetaData: Boolean);

Var
  O : TJSONObject;
  SS : TStringStream;
  N : String;
  I : Integer;
  M : TJSONobject;

begin
  O:=TJSONObject.Create;
  try
    N:='rows';
    If SaveMetaData then
      begin
      M:=MetaData;
      if M=Nil then
        M:=GenerateMetaData;
      O.Add('metaData',M);
      if M.IndexOfName('root')<>-1 then
        N:=M.Strings['root'];
      end;
    O.Add(N,Rows);
    SS:=TStringStream.Create(O.FormatJSON());
    try
      S.CopyFrom(SS,0);
    finally
      SS.Free;
    end;
  finally
    If (MetaData<>Nil) and SaveMetaData then
      begin
      I:=O.IndexOfName('metaData');
      if (I<>-1) then
        O.Extract(i);
      end;
    O.Extract(O.IndexOfName(N));
    O.Free;
  end;
end;

procedure TExtJSJSONDataSet.SaveToFile(const AFileName: String;
  SaveMetaData: Boolean);

Var
  F : TFileStream;

begin
  F:=TFileStream.Create(AFileName,fmCreate);
  try
    SaveToStream(F,SaveMetaData);
  finally
    F.Free;
  end;
end;

procedure TExtJSJSONDataSet.MetaDataToFieldDefs;

Var
  A : TJSONArray;
  F : TJSONObject;
  MaxLen,I,J,FS : Integer;
  N,idf : String;
  ft: TFieldType;
  D : TJSONData;

begin
  FieldDefs.Clear;
  I:=Metadata.IndexOfName('fields');
  if (I=-1) or (MetaData.Items[i].JSONType<>jtArray) then
    Raise EJSONDataset.Create('Invalid metadata object');
  A:=Metadata.Arrays['fields'];
  For I:=0 to A.Count-1 do
    begin
    If (A.Types[i]<>jtObject) then
      Raise EJSONDataset.CreateFmt('Field definition %d in metadata (%s) is not an object',[i,A[i].AsJSON]);
    F:=A.Objects[i];
    J:=F.IndexOfName('name');
    If (J=-1) or (F.Items[J].JSONType<>jtString) then
      Raise EJSONDataset.CreateFmt('Field definition %d in has no or invalid name property',[i]);
    N:=F.Items[J].AsString;
    J:=F.IndexOfName('type');
    If (J=-1) then
      ft:=ftstring
    else If (F.Items[J].JSONType<>jtString) then
      Raise EJSONDataset.CreateFmt('Field definition %d in has invalid type property',[i])
    else
      ft:=StringToFieldType(F.Items[J].asString);
    if (ft=ftString) then
      begin
      fs:=F.Get('maxLen',0);
      if fs=0 then
        fs:=GetStringFieldLength(F,N,I)
      end
    else
      fs:=0;
    FieldDefs.Add(N,ft,fs);
    end;
  FFields:=A;
end;

function TExtJSJSONDataSet.GenerateMetaData: TJSONObject;

Var
  F : TJSONArray;
  O : TJSONObject;
  I,M : Integer;
  T : STring;

begin
  Result:=TJSONObject.Create;
  F:=TJSONArray.Create;
  Result.Add('fields',F);
  For I:=0 to FieldDefs.Count -1 do
    begin
    O:=TJSONObject.Create(['name',FieldDefs[i].name]);
    F.Add(O);
    M:=0;
    case FieldDefs[i].DataType of
      ftfixedwidechar,
      ftwideString,
      ftfixedchar,
      ftString:
        begin
        T:='string';
        M:=FieldDefs[i].Size;
        end;
      ftBoolean: T:='boolean';
      ftDate,
      ftTime,
      ftDateTime: T:='date';
      ftFloat: t:='float';
      ftSmallint,
      ftInteger,
      ftAutoInc,
      ftLargeInt,
      ftword: t:='int';
    else
      Raise EJSONDataset.CreateFmt('Unsupported field type : %s',[GetEnumName(TypeInfo(TFieldType),Ord(FieldDefs[i].DataType))]);
    end; // case
    O.Strings['type']:=t;
    if M<>0 then
      O.Integers['maxlen']:=M;
    end;
  Result.strings['root']:='rows';
end;

Function TExtJSJSONDataSet.ConvertDateFormat(const S : String) : String;

{ Not handled: N S w z W t L o O P T Z c U MS }

begin
  Result:=StringReplace(S,'y','yy',[rfReplaceall]);
  Result:=StringReplace(Result,'Y','yyyy',[rfReplaceall]);
  Result:=StringReplace(Result,'g','h',[rfReplaceall]);
  Result:=StringReplace(Result,'G','hh',[rfReplaceall]);
  Result:=StringReplace(Result,'F','mmmm',[rfReplaceall]);
  Result:=StringReplace(Result,'M','mmm',[rfReplaceall]);
  Result:=StringReplace(Result,'n','m',[rfReplaceall]);
  Result:=StringReplace(Result,'D','ddd',[rfReplaceall]);
  Result:=StringReplace(Result,'j','d',[rfReplaceall]);
  Result:=StringReplace(Result,'l','dddd',[rfReplaceall]);
  Result:=StringReplace(Result,'i','nn',[rfReplaceall]);
  Result:=StringReplace(Result,'u','zzz',[rfReplaceall]);
  Result:=StringReplace(Result,'a','am/pm',[rfReplaceall,rfIgnoreCase]);
  Result:=LowerCase(Result);
end;

procedure TExtJSJSONDataSet.InitDateTimeFields;

Var
  F : TJSONObject;
  FF : TField;
  I,J : Integer;
  Fmt : String;

begin
  If (FFields=Nil) then
    Exit;
  For I:=0 to FFields.Count-1 do
    begin
    F:=FFields.Objects[i];
    J:=F.IndexOfName('type');
    if (J<>-1) and (F.Items[J].JSONType=jtString) and (F.items[J].AsString='date') then
      begin
      J:=F.IndexOfName('dateFormat');
      if (J<>-1) and (F.Items[J].JSONType=jtString) then
         begin
         FMT:=ConvertDateFormat(F.Items[J].AsString);
         FF:=FindField(F.Strings['name']);
         if (FF<>Nil) and (FF.DataType in [ftDate,ftTime,ftDateTime]) and (FF.FieldKind=fkData) then
           begin
           if FF is TJSONDateField then
             TJSONDateField(FF).DateFormat:=Fmt
           else if FF is TJSONTimeField then
             TJSONTimeField(FF).TimeFormat:=Fmt
           else if FF is TJSONDateTimeField then
             TJSONDateTimeField(FF).DateTimeFormat:=Fmt;
           end;
         end;
      end;
    end;
end;


{ TJSONArrayDataSet }

function TExtJSJSONArrayDataSet.CreateFieldMapper: TJSONFieldMapper;
begin
  Result:=TJSONArrayFieldMapper.Create;
end;

{ TJSONObjectDataSet }

function TExtJSJSONObjectDataSet.CreateFieldMapper: TJSONFieldMapper;
begin
  Result:=TJSONObjectFieldMapper.Create;
end;

end.

