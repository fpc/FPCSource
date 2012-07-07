{$mode objfpc}
{$h+}
unit fpjsondataset;

interface

uses
  DB, typinfo, Classes, SysUtils, fpjson;

type

  { TJSONFieldMapper }
  // This class is responsible for mapping the field objects of the records.
  TJSONFieldMapper = Class(TObject)
    // Return row TJSONData instance with data for field 'FieldName' or 'FieldIndex'.
    Function GetJSONDataForField(Const FieldName : String; FieldIndex : Integer; Row : TJSONData) : TJSONData; virtual; abstract;
    // Same, but now based on TField.
    Function GetJSONDataForField(F : TField; Row : TJSONData) : TJSONData; virtual;
    // Set data for field 'FieldName' or 'FieldIndex' to supplied TJSONData instance in row
    procedure SetJSONDataForField(Const FieldName : String; FieldIndex : Integer; Row,Data : TJSONData); virtual; abstract;
    // Set data for field TField to supplied TJSONData instance
    procedure SetJSONDataForField(F : TField; Row,Data : TJSONData); virtual;
    // Create a new row.
    Function CreateRow : TJSONData; virtual; abstract;
  end;

  // JSON has no date/time type, so we use a string field.
  // ExtJS provides the date/time  format in it's field config: 'dateFormat'
  // The below field classes store this in the NNNFormat field.
  { TJSONDateField }

  TJSONDateField = Class(TDateField)
  private
    FDateFormat: String;
  Published
    Property DateFormat : String Read FDateFormat Write FDateFormat;
  end;

  { TJSONTimeField }

  TJSONTimeField = Class(TTimeField)
  private
    FTimeFormat: String;
  Published
    Property TimeFormat : String Read FTimeFormat Write FTimeFormat;
  end;

  { TJSONDateTimeField }

  TJSONDateTimeField = Class(TDateTimeField)
  private
    FDateTimeFormat: String;
  Published
    Property DateTimeFormat : String Read FDateTimeFormat Write FDateTimeFormat;
  end;

  { TBaseJSONDataSet }

  // basic JSON dataset. Does nothing ExtJS specific.
  TBaseJSONDataSet = class (TDataSet)
  private
    FMUS: Boolean;
    FOwnsData : Boolean;
    FDefaultList : TFPList;
    FCurrentList: TFPList;
    FRecordSize: Integer;
    FCurrent: Integer;
    // Possible metadata to configure fields from.
    FMetaData : TJSONObject;
    // This will contain the rows.
    FRows : TJSONArray;
    FFieldMapper : TJSONFieldMapper;
    // When editing, this object is edited.
    FEditRow : TJSONData;
    procedure SetMetaData(AValue: TJSONObject);
    procedure SetRows(AValue: TJSONArray);
  protected
    // dataset virtual methods
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(ABookmark: Pointer); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalInsert; override;
    procedure InternalEdit; override;
    procedure InternalCancel; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    function  GetFieldClass(FieldType: TFieldType): TFieldClass; override;
    function IsCursorOpen: Boolean; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function GetFieldData(Field: TField; Buffer: Pointer; NativeFormat : Boolean): Boolean; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer; NativeFormat : Boolean); override;
    function GetRecordCount: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    function GetRecNo: Integer; override;
  Protected
    // New methods.
    // Called when dataset is closed. If OwnsData is true, metadata and rows are freed.
    Procedure FreeData; virtual;
    // Fill default list.
    Procedure FillList; virtual;
    // Convert MetaData object to FieldDefs.
    Procedure MetaDataToFieldDefs; virtual; abstract;
    // Initialize Date/Time info in all date/time fields. Called during InternalOpen
    procedure InitDateTimeFields; virtual;
    // Convert JSON date S to DateTime for Field F
    function ConvertDateTimeField(S: String; F: TField): TDateTime; virtual;
    // Format JSON date to from DT for Field F
    function FormatDateTimeField(DT : TDateTime; F: TField): String; virtual;
    // Create fieldmapper. A descendent MUST implement this.
    Function CreateFieldMapper : TJSONFieldMapper; virtual; abstract;
    // If True, then the dataset will free MetaData and FRows when it is closed.
    Property OwnsData : Boolean Read FownsData Write FOwnsData;
    // set to true if unknown field types should be handled as string fields.
    Property MapUnknownToStringType : Boolean Read FMUS Write FMUS;
    // Metadata
    Property MetaData : TJSONObject Read FMetaData Write SetMetaData;
    // Rows
    Property Rows : TJSONArray Read FRows Write SetRows;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
  published
    Property FieldDefs;
    // redeclared data set properties
    property Active;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

  { TExtJSJSONDataSet }

  // Base for ExtJS datasets. It handles MetaData conversion.
  TExtJSJSONDataSet = Class(TBaseJSONDataset)
  Private
    FFields : TJSONArray;
  Protected
    Function GenerateMetaData : TJSONObject;
    function ConvertDateFormat(S: String): String; virtual;
    Procedure MetaDataToFieldDefs; override;
    procedure InitDateTimeFields; override;
    function StringToFieldType(S: String): TFieldType;virtual;
    function GetStringFieldLength(F: TJSONObject; AName: String; AIndex: Integer): integer; virtual;
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

  { TJSONObjectFieldMapper }
  // Fieldmapper to be used when the data is in an object
  TJSONObjectFieldMapper = Class(TJSONFieldMapper)
    procedure SetJSONDataForField(Const FieldName : String; FieldIndex : Integer; Row,Data : TJSONData); override;
    Function GetJSONDataForField(Const FieldName : String; FieldIndex : Integer; Row : TJSONData) : TJSONData; override;
    Function CreateRow : TJSONData; override;
  end;

  { TJSONArrayFieldMapper }
  // Fieldmapper to be used when the data is in an array
  TJSONArrayFieldMapper = Class(TJSONFieldMapper)
    procedure SetJSONDataForField(Const FieldName : String; FieldIndex : Integer; Row,Data : TJSONData); override;
    Function GetJSONDataForField(Const FieldName : String; FieldIndex : Integer; Row : TJSONData) : TJSONData; override;
    Function CreateRow : TJSONData; override;
  end;

  EJSONDataset = Class(EDatabaseError);
  
implementation

uses dateutils, jsonparser;

type
  PRecInfo = ^TRecInfo;
  TRecInfo = record
    Index: Integer;
    Bookmark: Longint;
    BookmarkFlag: TBookmarkFlag;
  end;


{ TJSONFieldMapper }

function TJSONFieldMapper.GetJSONDataForField(F: TField; Row: TJSONData
  ): TJSONData;
begin
  // This supposes that Index is correct, i.e. the field positions have not been changed.
  Result:=GetJSONDataForField(F.FieldName,F.Index,Row);
end;

procedure TJSONFieldMapper.SetJSONDataForField(F: TField; Row,Data: TJSONData);
begin
  SetJSONDataForField(F.FieldName,F.Index,Row,Data);
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

{ TJSONArrayFieldMapper }

procedure TJSONArrayFieldMapper.SetJSONDataForField(const FieldName: String;
  FieldIndex: Integer; Row, Data: TJSONData);
begin
  (Row as TJSONArray).Items[FieldIndex]:=Data;
end;

function TJSONArrayFieldMapper.GetJSONDataForField(Const FieldName: String;
  FieldIndex: Integer; Row: TJSONData): TJSONData;
begin
  Result:=(Row as TJSONArray).Items[FieldIndex];
end;

function TJSONArrayFieldMapper.CreateRow: TJSONData;
begin
  Result:=TJSONArray.Create;
end;

{ TJSONObjectFieldMapper }

procedure TJSONObjectFieldMapper.SetJSONDataForField(const FieldName: String;
  FieldIndex: Integer; Row, Data: TJSONData);
begin
  (Row as TJSONObject).Elements[FieldName]:=Data;
end;

function TJSONObjectFieldMapper.GetJSONDataForField(const FieldName: String;
  FieldIndex: Integer; Row: TJSONData): TJSONData;
begin
  Result:=(Row as TJSONObject).Elements[FieldName];
end;

function TJSONObjectFieldMapper.CreateRow: TJSONData;
begin
  Result:=TJSONObject.Create;
end;

procedure TBaseJSONDataSet.SetMetaData(AValue: TJSONObject);
begin
  CheckInActive;
  if FMetaData=AValue then
    Exit;
  If OwnsData then
    FreeAndNil(FMetaData);
  FMetaData:=AValue;
end;

procedure TBaseJSONDataSet.SetRows(AValue: TJSONArray);
begin
  CheckInActive;
  if FRows=AValue then Exit;
  If OwnsData then
    FreeAndNil(FRows);
  FRows:=AValue;
end;

function TBaseJSONDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  Result := TRecordBuffer(StrAlloc(fRecordSize));
end;

// the next two are particularly ugly.
procedure TBaseJSONDataSet.InternalInitRecord(Buffer: TRecordBuffer);
begin
  FillChar(Buffer^, FRecordSize, 0);
end;

procedure TBaseJSONDataSet.FreeRecordBuffer (var Buffer: TRecordBuffer);
begin
  StrDispose(pansichar(Buffer));
end;

procedure TBaseJSONDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PInteger(Data)^ := PRecInfo(Buffer)^.Bookmark;
end;

function TBaseJSONDataSet.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := PRecInfo(Buffer)^.BookmarkFlag;
end;

function TBaseJSONDataSet.GetRecNo: Integer;
begin
  Result := FCurrent + 1;
end;

procedure TBaseJSONDataSet.InternalInitFieldDefs;
begin
  If Assigned(FMetaData) then
    MetaDataToFieldDefs;
  if (FieldDefs.Count=0) then
    Raise EJSONDataset.Create('No fields found');
end;

procedure TBaseJSONDataSet.FreeData;
begin
  If FOwnsData then
    begin
    FreeAndNil(FRows);
    FreeAndNil(FMetaData);
    end;
  if (FCurrentList<>FDefaultList) then
    FreeAndNil(FCurrentList)
  else
    FCurrentList:=Nil;
  FreeAndNil(FDefaultList);
  FreeAndNil(FFieldMapper);
  FCurrentList:=Nil;
end;

procedure TBaseJSONDataSet.FillList;

Var
  I : Integer;

begin
  FDefaultList:=TFPList.Create;
  For I:=0 to FRows.Count-1 do
    FDefaultList.Add(FRows[i]);
  FCurrentList:=FDefaultList;
end;

Function  TExtJSJSONDataSet.StringToFieldType(S : String) : TFieldType;

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

Function  TExtJSJSONDataSet.GetStringFieldLength(F : TJSONObject; AName : String; AIndex : Integer) : integer;

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
    For I:=0 to FRows.Count-1 do
      begin
      D:=FFieldMapper.GetJSONDataForField(Aname,AIndex,FRows[i]);
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
  P : TJSONParser;
  D : TJSONData;
  O : TJSONObject;
  N : String;
  I : Integer;

begin
  P:=TJSONParser.Create(S);
  try
    D:=P.Parse;
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
  finally
    P.Free;
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
  I,J,FS : Integer;
  N,idf : String;
  ft: TFieldType;
  D : TJSONData;

begin
  FieldDefs.Clear;
  I:=FMetadata.IndexOfName('fields');
  if (I=-1) or (FMetaData.Items[i].JSONType<>jtArray) then
    Raise EJSONDataset.Create('Invalid metadata object');
  A:=FMetadata.Arrays['fields'];
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
      fs:=GetStringFieldLength(F,N,I)
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

Function TExtJSJSONDataSet.ConvertDateFormat(S : String) : String;

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

function TBaseJSONDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  Result := grOK; // default
  case GetMode of
    gmNext: // move on
      if fCurrent < fCurrentList.Count - 1 then
        Inc (fCurrent)
      else
        Result := grEOF; // end of file
    gmPrior: // move back
      if fCurrent > 0 then
        Dec (fCurrent)
      else
        Result := grBOF; // begin of file
    gmCurrent: // check if empty
      if fCurrent >= fCurrentList.Count then
        Result := grEOF;
  end;
  if Result = grOK then // read the data
    with PRecInfo(Buffer)^ do
    begin
      Index := fCurrent;
      BookmarkFlag := bfCurrent;
      Bookmark := fCurrent;
    end;
end;

function TBaseJSONDataSet.GetRecordCount: Integer;
begin
  Result := FCurrentList.Count;
end;

function TBaseJSONDataSet.GetRecordSize: Word;
begin
  Result := SizeOf(Integer); // actual data without house-keeping
end;


procedure TBaseJSONDataSet.InternalClose;
begin
  // disconnet and destroy field objects
  BindFields (False);
  if DefaultFields then
    DestroyFields;
  FreeData;
end;

procedure TBaseJSONDataSet.InternalDelete;

Var
  R : TJSONData;

begin
  R:=TJSONData(FCurrentList[FCurrent]);
  FCurrentList.Delete(FCurrent);
  if (FCurrent>=FCurrentList.Count) then
    Dec(FCurrent);
  FRows.Remove(R);
end;

procedure TBaseJSONDataSet.InternalFirst;
begin
  FCurrent := -1;
end;

procedure TBaseJSONDataSet.InternalGotoBookmark(ABookmark: Pointer);
begin
  if (ABookmark <> nil) then
    FCurrent := Integer (ABookmark);
end;

procedure TBaseJSONDataSet.InternalInsert;

Var
  I : Integer;
  D : TFieldDef;

begin
  FEditRow:=FFieldMapper.CreateRow;
  For I:=0 to FieldDefs.Count-1 do
    begin
    D:=FieldDefs[i];
    FFieldMapper.SetJSONDataForField(D.Name,D.Index,FEditRow,TJSONNull.Create);
    end;
end;

procedure TBaseJSONDataSet.InternalEdit;
begin
  FEditRow:=TJSONData(FCurrentList[FCurrent]).Clone;
end;

procedure TBaseJSONDataSet.InternalCancel;
begin
  FreeAndNil(FEditRow);
end;

procedure TBaseJSONDataSet.InternalLast;
begin
  FCurrent:=FCurrentList.Count-1;
end;

procedure TBaseJSONDataSet.InitDateTimeFields;

begin
  // Do nothing
end;

procedure TBaseJSONDataSet.InternalOpen;
begin
  FreeAndNil(FFieldMapper);
  FFieldMapper:=CreateFieldMapper;
  IF (FRows=Nil) then // opening from fielddefs ?
    begin
    FRows:=TJSONArray.Create;
    OwnsData:=True;
    end;
  FillList;
  InternalInitFieldDefs;
  if DefaultFields then
    CreateFields;
  BindFields (True);
  InitDateTimeFields;
  FRecordSize := sizeof (TRecInfo);
  FCurrent := -1;
  BookmarkSize := sizeOf (Integer);
end;

procedure TBaseJSONDataSet.InternalPost;

Var
  RI,I : integer;
begin
  GetBookMarkData(ActiveBuffer,@I);
  if (State=dsInsert) then
    begin // Insert or Append
    FRows.Add(FEditRow);
    if GetBookMarkFlag(ActiveBuffer)=bfEOF then
      begin // Append
      FDefaultList.Add(FEditRow);
      if (FCurrentList<>FDefaultList) then
        FCurrentList.Add(FEditRow);
      end
    else  // insert
      begin
      FCurrentList.Insert(FCurrent,FEditRow);
      if (FCurrentList<>FDefaultList) then
        FDefaultList.Add(FEditRow);
      end;
    end
  else
    begin // Edit
    RI:=FRows.IndexOf(TJSONData(FCurrentList[FCurrent]));
    if (RI<>-1) then
      FRows[RI]:=FEditRow
    else
      FRows.Add(FEditRow);
    FCurrentList[FCurrent]:=FEditRow;
    if (FCurrentList<>FDefaultList) then
      FDefaultList[FCurrent]:=FEditRow;
    end;
  FEditRow:=Nil;
end;

procedure TBaseJSONDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  FCurrent := PRecInfo(Buffer)^.Index;
end;

function TBaseJSONDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;
begin
  case FieldType of
    ftDate : Result:=TJSONDateField;
    ftDateTime : Result:=TJSONDateTimeField;
    ftTime : Result:=TJSONTimeField;
  else
    Result:=inherited GetFieldClass(FieldType);
  end;
end;

function TBaseJSONDataSet.IsCursorOpen: Boolean;
begin
  Result := Assigned(FDefaultList);
end;

procedure TBaseJSONDataSet.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PRecInfo(Buffer)^.Bookmark := PInteger(Data)^;
end;

function TBaseJSONDataSet.ConvertDateTimeField(S : String; F : TField) : TDateTime;

Var
  Ptrn : string;

begin
  Result:=0;
  Case F.DataType of
    ftDate : Ptrn:=TJSONDateField(F).DateFormat;
    ftTime : Ptrn:=TJSONTimeField(F).TimeFormat;
    ftDateTime : Ptrn:=TJSONDateTimeField(F).DateTimeFormat;
  end;
  If (Ptrn='') then
    Case F.DataType of
      ftDate : Result:=StrToDate(S);
      ftTime : Result:=StrToTime(S);
      ftDateTime : Result:=StrToDateTime(S);
    end
  else
    begin
    Result:=ScanDateTime(ptrn,S,1);
    end;
end;

function TBaseJSONDataSet.FormatDateTimeField(DT: TDateTime; F: TField
  ): String;

Var
  Ptrn : string;
begin
  Result:='';
  Case F.DataType of
    ftDate : Ptrn:=TJSONDateField(F).DateFormat;
    ftTime : Ptrn:=TJSONTimeField(F).TimeFormat;
    ftDateTime : Ptrn:=TJSONDateTimeField(F).DateTimeFormat;
  end;
  If (Ptrn='') then
    Case F.DataType of
      ftDate : Result:=DateToStr(DT);
      ftTime : Result:=TimeToStr(DT);
      ftDateTime : Result:=DateTimeToStr(DT);
    end
  else
    Result:=FormatDateTime(ptrn,DT);
end;

function TBaseJSONDataSet.GetFieldData(Field: TField; Buffer: Pointer
  ; NativeFormat : Boolean): Boolean;
var
  R,F : TJSONData;
  B : WordBool;
  s: string;
  w : widestring;
  D : TDateTime;
  FV : Double;
  I : Longint;
  li : int64;

begin
  I:=PRecInfo(ActiveBuffer)^.Index;
  // Writeln('Index : ',I,'<',FCurrentList.Count,' ?');
  if (I<>-1) then
    R:=TJSONData(FCurrentList[i])
  else
    R:=FEditRow;
  F:=FFieldMapper.GetJSONDataForField(Field,R);
  Result:=(F<>Nil) and not (F.JSONType in [jtUnknown,jtNull]);
  if not Result then
    exit;
  case Field.DataType of
    ftfixedwidechar,
    ftwideString:
    begin
    W:=F.AsString;
    if (length(W)>0) then
      Move(W[1],Buffer^,Length(W)*SizeOf(Widechar)+1)
    else
      PChar(Buffer)^:=#0;
    end;
    ftfixedchar,
    ftString:
      begin
      S:=F.AsString;
      if (length(s)>0) then
        Move(S[1],Buffer^,Length(S)+1)
      else
        PChar(Buffer)^:=#0;
      end;
    ftBoolean:
      begin
      B:=F.AsBoolean;
      Move(B,Buffer^,sizeof(WordBool));
      end;
    ftDate,
    ftTime,
    ftDateTime:
      begin
      D:=ConvertDateTimeField(F.AsString,Field);
      Move(D,Buffer^,sizeof(TDateTime));
      end;
    ftFloat:
      begin
      Fv:=F.asFloat;
      Move(FV,Buffer^,sizeof(Double));
      end;
    ftSmallint,
    ftInteger,
    ftAutoInc,
    ftword:
      begin
      I:=F.AsInteger;
      Move(I,Buffer^,SizeOf(I));
      end;
    ftLargeint:
      begin
      LI:=F.AsInt64;
      Move(LI,Buffer^,SizeOf(LI));
      end;
  else
    Raise EJSONDataset.CreateFmt('Unsupported field type : %s',[GetEnumName(TypeInfo(TFieldType),Ord(Field.DataType))]);
  end; // case
end;

procedure TBaseJSONDataSet.SetFieldData(Field: TField; Buffer: Pointer; NativeFormat : Boolean);
var
  R,F : TJSONData;
  B : PWordBool;
  s: string;
  w : widestring;
  D : TDateTime;
  FV : Double;
  I : Longint;
  li : int64;

begin
  F:=Nil;
  if (Buffer<>nil) then
  case Field.DataType of
    ftfixedwidechar,
    ftwideString:
    begin
    SetLength(W,Field.Size);
    if (length(W)>0) then
      Move(Buffer^,W[1],Field.Size*SizeOf(Widechar));
    F:=TJSONString.Create(W);
    end;
    ftfixedchar,
    ftString:
      F:=TJSONString.Create(StrPas(Buffer));
    ftBoolean:
      F:=TJSONBoolean.Create(PWordBool(Buffer)^);
    ftDate,
    ftTime,
    ftDateTime:
      begin
      S:=FormatDateTimeField(PDateTime(Buffer)^,Field);
      F:=TJSONString.Create(S);
      end;
    ftFloat:
      F:=TJSONFloatNumber.Create(PDouble(Buffer)^);
    ftSmallint,
    ftInteger,
    ftAutoInc,
    ftword:
      F:=TJSONIntegerNumber.Create(PLongint(Buffer)^);
    ftLargeint:
      begin
      F:=TJSONInt64Number.Create(PInt64(Buffer)^);
      end;
  else
    Raise EJSONDataset.CreateFmt('Unsupported field type : %s',[GetEnumName(TypeInfo(TFieldType),Ord(Field.DataType))]);
  end; // case
  if (F=Nil) then
    F:=TJSONNull.Create;
//  Writeln('Set field data : ',F.AsJSON);
  FFieldMapper.SetJSONDataForField(Field,FEditRow,F);
//  Writeln('Field data is set : ',FEditRow.AsJSON);
end;

procedure TBaseJSONDataSet.SetBookmarkFlag(Buffer: TRecordBuffer;
  Value: TBookmarkFlag);
begin
  PRecInfo(Buffer)^.BookmarkFlag := Value;
end;

procedure TBaseJSONDataSet.SetRecNo(Value: Integer);
begin
  if (Value < 0) or (Value > FCurrentList.Count) then
    raise EJSONDataset.CreateFmt('SetRecNo: index %d out of range',[Value]);
  FCurrent := Value - 1;
  Resync([]); 
  DoAfterScroll;
end;

constructor TBaseJSONDataSet.Create(AOwner: TComponent);
begin
  inherited;
  FownsData:=True;
end;

destructor TBaseJSONDataSet.Destroy;
begin
  FreeData;
  inherited;
end;

end.
