{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canneyt and other members of the
    Free Pascal development team

    Field map implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit fieldmap;
{$mode objfpc}
{$H+}
interface

uses SysUtils,Classes, fmtBCD, db;

{ ---------------------------------------------------------------------
  TFieldMap
  ---------------------------------------------------------------------}

type
  EFieldMap = Class(EDatabaseError);

  { TFieldMap }

  TFieldMap = Class(TObject)
  private
    FDataset: TDataset;
    FFreeDataset: Boolean;
    FOldOnOpen : TDataSetNotifyEvent;
  Protected
    Procedure DoOnOpen(Sender : TDataset);
    Function FindField(const FN : String) : TField;
    Function FieldByName(const FN : String) : TField;
  Public
    Constructor Create(ADataset : TDataset; HookOnOpen : Boolean = False);
    Destructor Destroy; override;
    Procedure InitFields; virtual; abstract;
    Procedure LoadObject(AObject : TObject); virtual;
    Function GetFromField(F : TField; ADefault : TBCD) : TBCD; overload;
    Function GetFromField(F : TField; ADefault : Integer) : Integer; overload;
    Function GetFromField(F : TField; const ADefault : String) : String; overload;
    Function GetFromField(F : TField; ADefault : Boolean) : Boolean; overload;
    Function GetFromDateTimeField(F : TField; ADefault : TDateTime) : TDateTime; overload;
    Function GetFromField(F : TField; ADefault : Double) : Double; overload;
    Function GetFromField(F : TField; ADefault : Single) : Single; overload;
    Function GetFromField(F : TField; ADefault : Int64) : Int64; overload;
    Function GetFromField(F : TField; ADefault : LongWord) : LongWord; overload;
    Function GetFromField(F : TField; ADefault : Currency) : Currency; overload;
    Function GetFromField(F : TField; const ADefault : UnicodeString) : UnicodeString; overload;
    Function GetFromField(F : TField; const ADefault : WideString) : WideString; overload;
    Function GetFromField(F : TField; ADefault : TBytes) : TBytes; overload;
    Property Dataset : TDataset Read FDataset;
    Property FreeDataset : Boolean Read FFreeDataset Write FFreeDataset;
  end;
  TFieldMapClass = Class of TFieldMap;

  { TParamMap }

  TParamMap = Class(TObject)
  private
    FParams: TParams;
  Protected
    Function FindParam(const FN : String) : TParam;
    Function ParamByName(const FN : String) : TParam;
  Public
    Constructor Create(AParams : TParams);
    Procedure InitParams; virtual; abstract;
    Procedure SaveObject(AObject : TObject); virtual; abstract;
    Property Params : TParams Read FParams;
  end;

{ $INTERFACES CORBA}

  ITypeSafeDatasetAccess = Interface ['{67496051-66AA-474E-9CB2-A4AEAA7A2324}']
    // Property getter/setter
    procedure SetFreeDataset(AValue: Boolean);
    function GetFreeDataset: Boolean;
    function GetActive: boolean;
    function GetIsEmpty: boolean;
    function GetModified: Boolean;
    function GetRecNo: integer;
    function GetRecordCount: integer;
    function GetState: TDatasetState;
    function GetBOF: Boolean;
    function GetDataset: TDataset;
    function GetEOF: Boolean;
    procedure SetActive(AValue: boolean);
    procedure SetRecNo(AValue: integer);
    // Examine data
    function IsFieldNull(const FieldName : String) : boolean;
    function IsFieldNull(const FieldIndex : Integer) : boolean;
    // Open/close
    procedure Open;
    procedure Close;
    // Navigation
    procedure First;
    procedure Prior;
    Procedure Next;
    procedure Last;
    function Locate(const aKeyFields: string; const aKeyValues: Variant; aOptions: TLocateOptions = []): Boolean;
    function Lookup(const aKeyFields: string; const aKeyValues: Variant; const aResultFields: string): Variant;
    // Modification
    Procedure Append;
    Procedure Insert;
    Procedure Edit;
    Procedure Post;
    Procedure Delete;
    Procedure Cancel;
    Procedure ApplyUpdates;
    procedure ClearField(const FieldName : String);
    procedure ClearField(const FieldIndex : Integer);
    // Properties
    Property EOF : Boolean Read GetEOF;
    Property BOF : Boolean Read GetBOF;
    Property Dataset : TDataset Read GetDataset;
    property IsEmpty : boolean read GetIsEmpty;
    property State : TDatasetState read GetState;
    property RecordCount: integer read GetRecordCount;
    property RecNo : integer read GetRecNo write SetRecNo;
    property Active: boolean read GetActive write SetActive;
    Property Modified : Boolean Read GetModified;
    Property FreeDataset : Boolean Read GetFreeDataset Write SetFreeDataset;
  end;

  { TTypeSafeDatasetAccess }

  TTypeSafeDatasetAccess = Class(TInterfacedObject, ITypeSafeDatasetAccess)
  private
    FFieldMap: TFieldMap;
    function GetActive: boolean;
    function GetIsEmpty: boolean;
    function GetModified: Boolean;
    function GetFreeDataset: Boolean;
    function GetRecNo: integer;
    function GetRecordCount: integer;
    function GetState: TDatasetState;
    procedure SetActive(AValue: boolean);
    procedure SetFreeDataset(AValue: Boolean);
    procedure SetRecNo(AValue: integer);
  Protected
    Class Function FieldMapClass : TFieldMapClass; virtual; abstract;
    function GetBOF: Boolean;
    function GetDataset: TDataset;
    function GetEOF: Boolean;
    Property FieldMap : TFieldMap Read FFieldMap;
  Public
    Constructor Create(aDataset : TDataset; HookOnOpen : Boolean = True);
    Destructor Destroy; override;
    function  IsFieldNull(const FieldName : String) : boolean;
    function  IsFieldNull(const FieldIndex : Integer) : boolean;
    procedure ClearField(const FieldName : String);
    procedure ClearField(const FieldIndex : Integer);
    // Open/close
    procedure Open;
    procedure Close;
    // Navigation
    procedure First;
    procedure Prior;
    Procedure Next;
    procedure Last;
    function Locate(const aKeyFields: string; const aKeyValues: Variant; aOptions: TLocateOptions = []): Boolean;
    function Lookup(const aKeyFields: string; const aKeyValues: Variant;
      const aResultFields: string): Variant;
    // Modification
    Procedure Append;
    Procedure Insert;
    Procedure Edit;
    Procedure Post;
    Procedure Delete;
    Procedure Cancel;
    Procedure ApplyUpdates; virtual;

    Property EOF : Boolean Read GetEOF;
    Property BOF : Boolean Read GetBOF;
    Property Dataset : TDataset Read GetDataset;
    property IsEmpty : boolean read GetIsEmpty;
    property State : TDatasetState read GetState;
    property RecordCount: integer read GetRecordCount;
    property RecNo : integer read GetRecNo write SetRecNo;
    property Active: boolean read GetActive write SetActive;
    Property Modified : Boolean Read GetModified;
    Property FreeDataset : Boolean Read GetFreeDataset Write SetFreeDataset;
  end;

  { TBlobProxyStream }

  TBlobProxyStream = Class(TOwnerStream)
  Private
    FChangeCount : Integer;
    FOnChange: TNotifyEvent;
    function GetUpdating: Boolean;
  Protected
    Procedure DoChanged; virtual;
    Procedure BeginUpdate; virtual;
    Procedure EndUpdate; virtual;
    function GetSize: Int64; override;
    procedure SetSize(const aValue: Int64); override;
    function GetPosition: Int64; override;
    procedure SetPosition(const aValue: Int64); override;
  Public
    Constructor create; overload;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override; overload;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    Property Updating : Boolean Read GetUpdating;
  end;


implementation


resourcestring
  SErrNoDataset = '%s: No dataset available.';
  SErrNoParamsForParam  = '%s: No params to search param "%s".';
  SErrNoObjectToLoad = '%s: No object to load';


function TBlobProxyStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := Source.Seek(Offset, Origin);
end;

procedure TBlobProxyStream.SetPosition(const aValue: Int64);
begin
  Source.Position := aValue;
end;

procedure TBlobProxyStream.SetSize(const aValue: Int64);
begin
  Source.Size := aValue;
  DoChanged;
end;

function TBlobProxyStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Source.Write(Buffer, Count);
  DoChanged;
end;

procedure TBlobProxyStream.BeginUpdate;
begin
  inc(FChangeCount);
end;

constructor TBlobProxyStream.create;
begin
  Inherited Create(TMemoryStream.Create);
  SourceOwner:=True;
end;

procedure TBlobProxyStream.EndUpdate;
begin
  if FChangeCount > 0 then
    Dec(FChangeCount);
  DoChanged;
end;

function TBlobProxyStream.GetPosition: Int64;
begin
  Result := Source.Position;
end;

function TBlobProxyStream.GetSize: Int64;
begin
  Result := Source.Size;
end;

function TBlobProxyStream.GetUpdating: Boolean;
begin
  Result:=FChangeCount>0;
end;

procedure TBlobProxyStream.DoChanged;
begin
  if (FChangeCount = 0) and Assigned(OnChange) then
    OnChange(Self);
end;

function TBlobProxyStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := Source.Read(Buffer, Count);
end;

{ TTypeSafeDatasetAccess }

function TTypeSafeDatasetAccess.GetIsEmpty: boolean;
begin
  With Dataset do
    Result:=EOF and BOF;
end;

function TTypeSafeDatasetAccess.GetModified: Boolean;
begin
  Result:=Dataset.Modified;
end;

function TTypeSafeDatasetAccess.GetFreeDataset: Boolean;
begin
  Result:=FFieldMap.FreeDataset;
end;

function TTypeSafeDatasetAccess.GetActive: boolean;
begin
  Result:=Dataset.Active;
end;

function TTypeSafeDatasetAccess.GetRecNo: integer;
begin
  Result:=Dataset.RecNo;
end;

function TTypeSafeDatasetAccess.GetRecordCount: integer;
begin
  Result:=Dataset.RecordCount
end;

function TTypeSafeDatasetAccess.GetState: TDatasetState;
begin
  Result:=Dataset.State;
end;

procedure TTypeSafeDatasetAccess.SetActive(AValue: boolean);
begin
  Dataset.Active:=AValue;
end;

procedure TTypeSafeDatasetAccess.SetFreeDataset(AValue: Boolean);
begin
  FFieldMap.FreeDataset:=AValue;
end;

procedure TTypeSafeDatasetAccess.SetRecNo(AValue: integer);
begin
  Dataset.RecNo:=AValue;
end;

function TTypeSafeDatasetAccess.GetBOF: Boolean;
begin
  Result:=Dataset.BOF;
end;

function TTypeSafeDatasetAccess.GetDataset: TDataset;
begin
  Result:=FieldMap.Dataset;
end;

function TTypeSafeDatasetAccess.GetEOF: Boolean;
begin
  Result:=Dataset.EOF;
end;

procedure TTypeSafeDatasetAccess.ApplyUpdates;
begin
  // Needs to be implemented by descendents
end;

constructor TTypeSafeDatasetAccess.Create(aDataset: TDataset; HookOnOpen : Boolean = True);
begin
  FFieldMap:=FieldMapClass.Create(aDataset,HookOnOpen);
end;

destructor TTypeSafeDatasetAccess.Destroy;
begin
  FreeAndNil(FFieldMap);
  inherited Destroy;
end;

function TTypeSafeDatasetAccess.IsFieldNull(const FieldName: String): boolean;
begin
  Result:=Dataset.FieldByName(FieldName).IsNull;
end;

function TTypeSafeDatasetAccess.IsFieldNull(const FieldIndex: Integer): boolean;
begin
  Result:=Dataset.Fields[FieldIndex].IsNull;
end;

procedure TTypeSafeDatasetAccess.ClearField(const FieldName: String);
begin
  Dataset.FieldByName(FieldName).Clear;
end;

procedure TTypeSafeDatasetAccess.ClearField(const FieldIndex: Integer);
begin
  Dataset.Fields[FieldIndex].Clear;
end;

procedure TTypeSafeDatasetAccess.Open;
begin
  Dataset.Open;
end;

procedure TTypeSafeDatasetAccess.Close;
begin
  Dataset.Close;
end;

procedure TTypeSafeDatasetAccess.First;
begin
  Dataset.First;
end;

procedure TTypeSafeDatasetAccess.Append;
begin
  Dataset.Append;
end;

procedure TTypeSafeDatasetAccess.Insert;
begin
  Dataset.Insert;
end;

procedure TTypeSafeDatasetAccess.Edit;
begin
  Dataset.Edit;
end;

procedure TTypeSafeDatasetAccess.Next;
begin
  Dataset.Next;
end;

procedure TTypeSafeDatasetAccess.Last;
begin
  Dataset.Last;
end;

function TTypeSafeDatasetAccess.Locate(const aKeyFields: string;
  const aKeyValues: Variant; aOptions: TLocateOptions): Boolean;
begin
  Result:=Dataset.Locate(aKeyFields,AKeyValues,aOptions);
end;

function TTypeSafeDatasetAccess.Lookup(const aKeyFields: string;
  const aKeyValues: Variant; const aResultFields: string): Variant;
begin
  Result:=Dataset.Lookup(aKeyFields,aKeyValues,aResultFields);
end;

procedure TTypeSafeDatasetAccess.Prior;
begin
  Dataset.Prior;
end;

procedure TTypeSafeDatasetAccess.Post;
begin
  Dataset.Post;
end;

procedure TTypeSafeDatasetAccess.Delete;
begin
  Dataset.Delete;
end;

procedure TTypeSafeDatasetAccess.Cancel;
begin
  Dataset.Cancel;
end;

{ TParamMap }

function TParamMap.FindParam(const FN: String): TParam;
begin
  Result:=FParams.FindParam(FN);
  {if (Result=Nil) then
    Writeln(ClassName,' param ',FN,' not found');}
end;

function TParamMap.ParamByName(const FN: String): TParam;
begin
  If (FParams=Nil) then
    Raise Exception.CreateFmt(SErrNoParamsForParam,[ClassName,FN]);
  Result:=FParams.ParamByName(FN);
end;

constructor TParamMap.Create(AParams: TParams);
begin
  FParams:=AParams;
  InitParams;
end;

{ TFieldMap }

constructor TFieldMap.Create(ADataset: TDataset; HookOnOpen : Boolean = False);
begin
  if (ADataset=Nil) then
    Raise EFieldMap.CreateFmt(SErrNoDataset,[ClassName]);
  FDataset:=ADataset;
  if HookOnOpen then
    begin
    FOldOnOpen:=FDataset.AfterOpen;
    FDataset.AfterOpen:=@DoOnOpen;
    end;
  if FDataset.Active then
    InitFields;
end;

destructor TFieldMap.Destroy;
begin
  if FFreeDataset then
    FreeAndNil(FFreeDataset);
  inherited Destroy;
end;

procedure TFieldMap.LoadObject(AObject: TObject);
begin
  If (AObject=Nil) then
    Raise EFieldMap.CreateFmt(SErrNoObjectToLoad,[ClassName]);
end;

function TFieldMap.GetFromField(F: TField; ADefault: TBCD): TBCD;
begin
  If Assigned(F) then
      Result:=F.AsBCD
    else
      Result:=ADefault;
end;

function TFieldMap.FieldByName(const FN: String): TField;
begin
  Result:=FDataset.FieldByName(FN)
end;

procedure TFieldMap.DoOnOpen(Sender: TDataset);
begin
  InitFields;
  If Assigned(FOldOnOpen) then
    FOldOnOpen(Sender);
end;

function TFieldMap.FindField(const FN: String): TField;
begin
  If (FDataset=Nil) then
    Result:=Nil
  else
    Result:=FDataset.FindField(FN);
end;

function TFieldMap.GetFromField(F: TField; ADefault: Integer): Integer;
begin
  If Assigned(F) then
    Result:=F.AsInteger
  else
    Result:=ADefault;
end;

function TFieldMap.GetFromField(F: TField; const ADefault: String): String;
begin
  If Assigned(F) then
    Result:=F.AsString
  else
    Result:=ADefault;
end;

function TFieldMap.GetFromField(F: TField; ADefault: Boolean): Boolean;
begin
  If Assigned(F) then
    begin
    if (F is TStringField) then
      Result:=(F.AsString='+')
    else
      Result:=F.AsBoolean
    end
  else
    Result:=ADefault;
end;

function TFieldMap.GetFromDateTimeField(F: TField; ADefault: TDateTime): TDateTime;
begin
  If Assigned(F) then
    Result:=F.AsDateTime
  else
    Result:=ADefault;
end;

function TFieldMap.GetFromField(F: TField; ADefault: Double): Double;
begin
  If Assigned(F) then
    if F.DataType in [ftDate,ftDateTime,ftTime,ftTimeStamp] then
      Result:=F.AsDateTime
    else
      Result:=F.AsFloat
  else
    Result:=ADefault;
end;

function TFieldMap.GetFromField(F: TField; ADefault: Single): Single;
begin
  If Assigned(F) then
    Result:=F.AsSingle
  else
    Result:=ADefault;
end;

function TFieldMap.GetFromField(F: TField; ADefault: Int64): Int64;
begin
  If Assigned(F) then
    Result:=F.AsLargeInt
  else
    Result:=ADefault;
end;

function TFieldMap.GetFromField(F: TField; ADefault: LongWord): LongWord;
begin
  If Assigned(F) then
    Result:=F.AsLongWord
  else
    Result:=ADefault;
end;

function TFieldMap.GetFromField(F: TField; ADefault: Currency): Currency;
begin
  If Assigned(F) then
    Result:=F.AsCurrency
  else
    Result:=ADefault;
end;

function TFieldMap.GetFromField(F: TField; const ADefault: UnicodeString): UnicodeString;
begin
  If Assigned(F) then
    Result:=F.AsUnicodeString
  else
    Result:=ADefault;
end;

function TFieldMap.GetFromField(F: TField; const ADefault: WideString): WideString;
begin
  If Assigned(F) then
    Result:=F.AsWideString
  else
    Result:=ADefault;
end;

function TFieldMap.GetFromField(F: TField; ADefault: TBytes): TBytes;
begin
  If Assigned(F) then
    Result:=F.AsBytes
  else
    Result:=ADefault;
end;

end.
 
