{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt, member of the
    Free Pascal development team


    DB header file with interface section.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit db;

{$mode objfpc}
{$h+}

interface

uses Classes,Sysutils;

const

  dsMaxBufferCount = MAXINT div 8;
  dsMaxStringSize = 8192;

  // Used in AsBoolean for string fields to determine
  // whether it's true or false.
  YesNoChars : Array[Boolean] of char = ('Y','N');

type
{LargeInt}
  LargeInt = Int64;

{ Auxiliary type }
  TStringFieldBuffer = Array[0..dsMaxStringSize] of Char;

{ Misc Dataset types }

  TDataSetState = (dsInactive, dsBrowse, dsEdit, dsInsert, dsSetKey,
    dsCalcFields, dsFilter, dsNewValue, dsOldValue, dsCurValue);


  TDataEvent = (deFieldChange, deRecordChange, deDataSetChange,
    deDataSetScroll, deLayoutChange, deUpdateRecord, deUpdateState,
    deCheckBrowseMode, dePropertyChange, deFieldListChange, deFocusControl);

  TUpdateStatus = (usUnmodified, usModified, usInserted, usDeleted);

{ Forward declarations }

  TFieldDef = class;
  TFieldDefs = class;
  TField = class;
  TFields = Class;
  TDataSet = class;
  TDataBase = Class;
  TDatasource = Class;
  TDatalink = Class;

{ Exception classes }

  EDatabaseError = class(Exception);

{ TFieldDef }

  TFieldClass = class of TField;

{
  TFieldType = (ftUnknown, ftString, ftSmallint, ftInteger, ftWord,
    ftBoolean, ftFloat, ftDate, ftTime, ftDateTime,
    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic,
    ftFmtMemo, ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor);
}

  TFieldType = (ftUnknown, ftString, ftSmallint, ftInteger, ftWord,
    ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate,  ftTime, ftDateTime,
    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar,
    ftWideString, ftLargeint, ftADT, ftArray, ftReference,
    ftDataSet, ftOraBlob, ftOraClob, ftVariant, ftInterface,
    ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd);

  TFieldDef = class(TComponent)
  Private
    FDataType : TFieldType;
    FFieldNo : Longint;
    FInternalCalcField : Boolean;
    FPrecision : Longint;
    FRequired : Boolean;
    FSize : Word;
    FName : String;
    Function GetFieldClass : TFieldClass;
  public
    constructor Create(AOwner: TFieldDefs; const AName: string;
      ADataType: TFieldType; ASize: Word; ARequired: Boolean; AFieldNo: Longint);
    destructor Destroy; override;
    function CreateField(AOwner: TComponent): TField;
    property InternalCalcField: Boolean read FInternalCalcField write FInternalCalcField;
    property DataType: TFieldType read FDataType;
    property FieldClass: TFieldClass read GetFieldClass;
    property FieldNo: Longint read FFieldNo;
    property Name: string read FName;
    property Precision: Longint read FPrecision write FPrecision;
    property Required: Boolean read FRequired;
    property Size: Word read FSize;
  end;

{ TFieldDefs }

  TFieldDefs = class(TComponent)
  private
    FDataSet: TDataSet;
    FItems: TList;
    FUpdated: Boolean;
    function GetCount: Longint;
    function GetItem(Index: Longint): TFieldDef;
  public
    constructor Create(ADataSet: TDataSet);
    destructor Destroy; override;
    procedure Add(const AName: string; ADataType: TFieldType; ASize: Word;
      ARequired: Boolean);
    procedure Assign(FieldDefs: TFieldDefs);
    procedure Clear;
    function Find(const AName: string): TFieldDef;
    function IndexOf(const AName: string): Longint;
    procedure Update;
    property Count: Longint read GetCount;
    property Items[Index: Longint]: TFieldDef read GetItem; default;
  end;

{ TField }

  TFieldKind = (fkData, fkCalculated, fkLookup, fkInternalCalc);
  TFieldKinds = Set of TFieldKind;

  TFieldNotifyEvent = procedure(Sender: TField) of object;
  TFieldGetTextEvent = procedure(Sender: TField; var Text: string;
    DisplayText: Boolean) of object;
  TFieldSetTextEvent = procedure(Sender: TField; const Text: string) of object;
  TFieldRef = ^TField;
  TFieldChars = set of Char;
  { TAlignment may need to come from somewhere else }
  TAlignMent = (taLeftjustify,taCenter,taRightJustify);

  TField = class(TComponent)
  Private
    FAlignMent : TAlignment;
    FAttributeSet : String;
    FBuffers : ppchar;
    FCalculated : Boolean;
    FCanModify : Boolean;
    FConstraintErrorMessage : String;
    FCustomConstraint : String;
    FDataSet : TDataSet;
    FDataSize : Word;
    FDataType : TFieldType;
    FDefaultExpression : String;
    FDisplayLabel : String;
    FDisplayWidth : Longint;
    FEditText : String;
    FFieldKind : TFieldKind;
    FFieldName : String;
    FFieldNo : Longint;
    FFields : TFields;
    FHasConstraints : Boolean;
    FImportedConstraint : String;
    FIsIndexField : Boolean;
    FKeyFields : String;
    FLookupCache : Boolean;
    FLookupDataSet : TDataSet;
    FLookupKeyfields : String;
    FLookupresultField : String;
    FOffset : Word;
    FOnChange : TFieldNotifyEvent;
    FOnGetText: TFieldGetTextEvent;
    FOnSetText: TFieldSetTextEvent;
    FOnValidate: TFieldNotifyEvent;
    FOrigin : String;
    FReadOnly : Boolean;
    FRequired : Boolean;
    FSize : Word;
    FValidChars : TFieldChars;
    FValueBuffer : Pointer;
    FValidating : Boolean;
    FVisible : Boolean;
    Function GetIndex : longint;
    Procedure SetDataset(VAlue : TDataset);
    function GetDisplayText: String;
  protected
    function AccessError(const TypeName: string): EDatabaseError;
    procedure CheckInactive;
    class procedure CheckTypeSize(AValue: Longint); virtual;
    procedure Change; virtual;
    procedure DataChanged;
    procedure FreeBuffers; virtual;
    function GetAsBoolean: Boolean; virtual;
    function GetAsDateTime: TDateTime; virtual;
    function GetAsFloat: Extended; virtual;
    function GetAsLongint: Longint; virtual;
    function GetAsString: string; virtual;
    function GetCanModify: Boolean; virtual;
    function GetDataSize: Word; virtual;
    function GetDefaultWidth: Longint; virtual;
    function GetDisplayName : String;
    function GetIsNull: Boolean; virtual;
    function GetParentComponent: TComponent; override;
    procedure GetText(var AText: string; ADisplayText: Boolean); virtual;
    function HasParent: Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PropertyChanged(LayoutAffected: Boolean);
    procedure ReadState(Reader: TReader); override;
    procedure SetAsBoolean(AValue: Boolean); virtual;
    procedure SetAsDateTime(AValue: TDateTime); virtual;
    procedure SetAsFloat(AValue: Extended); virtual;
    procedure SetAsLongint(AValue: Longint); virtual;
    procedure SetAsString(const AValue: string); virtual;
    procedure SetDataType(AValue: TFieldType);
    procedure SetSize(AValue: Word); virtual;
    procedure SetParentComponent(AParent: TComponent); override;
    procedure SetText(const AValue: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; virtual;
    procedure FocusControl;
    function GetData(Buffer: Pointer): Boolean;
    class function IsBlob: Boolean; virtual;
    function IsValidChar(InputChar: Char): Boolean; virtual;
    procedure SetData(Buffer: Pointer);
    procedure SetFieldType(AValue: TFieldType); virtual;
    procedure Validate(Buffer: Pointer);
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    property AsLongint: Longint read GetAsLongint write SetAsLongint;
    property AsString: string read GetAsString write SetAsString;
    property AttributeSet: string read FAttributeSet write FAttributeSet;
    property Calculated: Boolean read FCalculated write FCalculated;
    property CanModify: Boolean read FCanModify;
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property DataSize: Word read GetDataSize;
    property DataType: TFieldType read FDataType;
    property DisplayName: String Read GetDisplayName;
    property DisplayText: String read GetDisplayText;
    property FieldNo: Longint read FFieldNo;
    property IsIndexField: Boolean read FIsIndexField;
    property IsNull: Boolean read GetIsNull;
    property Offset: word read FOffset;
    property Size: Word read FSize write FSize;
    property Text: string read FEditText write FEditText;
    property ValidChars : TFieldChars Read FValidChars;
  published
    property AlignMent : TAlignMent Read FAlignMent write FAlignment;
    property CustomConstraint: string read FCustomConstraint write FCustomConstraint;
    property ConstraintErrorMessage: string read FConstraintErrorMessage write FConstraintErrorMessage;
    property DefaultExpression: string read FDefaultExpression write FDefaultExpression;
    property DisplayLabel : string read FDisplayLabel write FDisplayLabel;
    property DisplayWidth: Longint read FDisplayWidth write FDisplayWidth;
    property FieldKind: TFieldKind read FFieldKind write FFieldKind;
    property FieldName: string read FFieldName write FFieldName;
    property HasConstraints: Boolean read FHasConstraints;
    property Index: Longint read GetIndex;
    property ImportedConstraint: string read FImportedConstraint write FImportedConstraint;
    property LookupDataSet: TDataSet read FLookupDataSet write FLookupDataSet;
    property LookupKeyFields: string read FLookupKeyFields write FLookupKeyFields;
    property LookupResultField: string read FLookupResultField write FLookupResultField;
    property KeyFields: string read FKeyFields write FKeyFields;
    property LookupCache: Boolean read FLookupCache write FLookupCache;
    property Origin: string read FOrigin write FOrigin;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property Required: Boolean read FRequired write FRequired;
    property Visible: Boolean read FVisible write FVisible;
    property OnChange: TFieldNotifyEvent read FOnChange write FOnChange;
    property OnGetText: TFieldGetTextEvent read FOnGetText write FOnGetText;
    property OnSetText: TFieldSetTextEvent read FOnSetText write FOnSetText;
    property OnValidate: TFieldNotifyEvent read FOnValidate write FOnValidate;
  end;

{ TStringField }

  TStringField = class(TField)
  protected
    class procedure CheckTypeSize(AValue: Longint); override;
    function GetAsBoolean: Boolean; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsFloat: Extended; override;
    function GetAsLongint: Longint; override;
    function GetAsString: string; override;
    function GetDataSize: Word; override;
    function GetDefaultWidth: Longint; override;
    procedure GetText(var AText: string; ADisplayText: Boolean); override;
    function GetValue(var AValue: string): Boolean;
    procedure SetAsBoolean(AValue: Boolean); override;
    procedure SetAsDateTime(AValue: TDateTime); override;
    procedure SetAsFloat(AValue: Extended); override;
    procedure SetAsLongint(AValue: Longint); override;
    procedure SetAsString(const AValue: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: string read GetAsString write SetAsString;
  published
    property Size default 20;
  end;

{ TNumericField }
  TNumericField = class(TField)
  Private
    FDisplayFormat : String;
    FEditFormat : String;
  protected
    procedure RangeError(AValue, Min, Max: Extended);
    procedure SetDisplayFormat(const AValue: string);
    procedure SetEditFormat(const AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
    property EditFormat: string read FEditFormat write SetEditFormat;
  end;

{ TLongintField }

  TLongintField = class(TNumericField)
  private
    FMinValue,
    FMaxValue,
    FMinRange,
    FMAxRange  : Longint;
    Procedure SetMinValue (AValue : longint);
    Procedure SetMaxValue (AValue : longint);
  protected
    function GetAsFloat: Extended; override;
    function GetAsLongint: Longint; override;
    function GetAsString: string; override;
    function GetDataSize: Word; override;
    procedure GetText(var AText: string; ADisplayText: Boolean); override;
    function GetValue(var AValue: Longint): Boolean;
    procedure SetAsFloat(AValue: Extended); override;
    procedure SetAsLongint(AValue: Longint); override;
    procedure SetAsString(const AValue: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    Function CheckRange(AValue : longint) : Boolean;
    property Value: Longint read GetAsLongint write SetAsLongint;
  published
    property MaxValue: Longint read FMaxValue write SetMaxValue default 0;
    property MinValue: Longint read FMinValue write SetMinValue default 0;
  end;
  TIntegerField = TLongintField;

{ TSmallintField }

  TSmallintField = class(TLongintField)
  protected
    function GetDataSize: Word; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TWordField }

  TWordField = class(TLongintField)
  protected
    function GetDataSize: Word; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TAutoIncField }

  TAutoIncField = class(TLongintField)
  Protected
    Procedure SetAsLongInt(AValue : Longint); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TFloatField }

  TFloatField = class(TNumericField)
  private
    FMaxValue : Extended;
    FMinValue : Extended;
    FPrecision : Longint;
  protected
    function GetAsFloat: Extended; override;
    function GetAsLongint: Longint; override;
    function GetAsString: string; override;
    function GetDataSize: Word; override;
    procedure GetText(var theText: string; ADisplayText: Boolean); override;
    procedure SetAsFloat(AValue: Extended); override;
    procedure SetAsLongint(AValue: Longint); override;
    procedure SetAsString(const AValue: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    Function CheckRange(AValue : Extended) : Boolean;
    property Value: Extended read GetAsFloat write SetAsFloat;

  published
    property MaxValue: Extended read FMaxValue write FMaxValue;
    property MinValue: Extended read FMinValue write FMinValue;
    property Precision: Longint read FPrecision write FPrecision default 15;
  end;


{ TBooleanField }

  TBooleanField = class(TField)
  private
    FDisplayValues : String;
    // First byte indicates uppercase or not.
    FDisplays : Array[Boolean,Boolean] of string;
    Procedure SetDisplayValues(AValue : String);
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsString: string; override;
    function GetDataSize: Word; override;
    function GetDefaultWidth: Longint; override;
    procedure SetAsBoolean(AValue: Boolean); override;
    procedure SetAsString(const AValue: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: Boolean read GetAsBoolean write SetAsBoolean;
  published
    property DisplayValues: string read FDisplayValues write SetDisplayValues;
  end;

{ TDateTimeField }

  TDateTimeField = class(TField)
  private
    FDisplayFormat : String;
  protected
    function GetAsDateTime: TDateTime; override;
    function GetAsFloat: Extended; override;
    function GetAsString: string; override;
    function GetDataSize: Word; override;
    procedure GetText(var theText: string; ADisplayText: Boolean); override;
    procedure SetAsDateTime(AValue: TDateTime); override;
    procedure SetAsFloat(AValue: Extended); override;
    procedure SetAsString(const AValue: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: TDateTime read GetAsDateTime write SetAsDateTime;
  published
    property DisplayFormat: string read FDisplayFormat write FDisplayFormat;
  end;

{ TDateField }

  TDateField = class(TDateTimeField)
  protected
    function GetDataSize: Word; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TTimeField }

  TTimeField = class(TDateTimeField)
  protected
    function GetDataSize: Word; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TBinaryField }

  TBinaryField = class(TField)
  protected
    class procedure CheckTypeSize(AValue: Longint); override;
    function GetAsString: string; override;
    procedure GetText(var TheText: string; ADisplayText: Boolean); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetText(const AValue: string); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Size default 16;
  end;

{ TBytesField }

  TBytesField = class(TBinaryField)
  protected
    function GetDataSize: Word; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TVarBytesField }

  TVarBytesField = class(TBytesField)
  protected
    function GetDataSize: Word; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TBCDField }

  TBCDField = class(TNumericField)
  private
  protected
    class procedure CheckTypeSize(AValue: Longint); override;
    function GetAsFloat: Extended; override;
    function GetAsLongint: Longint; override;
    function GetAsString: string; override;
    function GetDataSize: Word; override;
    function GetDefaultWidth: Longint; override;
    procedure GetText(var TheText: string; ADisplayText: Boolean); override;
    procedure SetAsFloat(AValue: Extended); override;
    procedure SetAsLongint(AValue: Longint); override;
    procedure SetAsString(const AValue: string); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Size default 4;
  end;

{ TBlobField }
  TBlobStreamMode = (bmRead, bmWrite, bmReadWrite);
  TBlobType = ftBlob..ftTypedBinary;

  TBlobField = class(TField)
  private
    FBlobSize : Longint;
    FBlobType : TBlobType;
    FModified : Boolean;
    FTransliterate : Boolean;
    Function GetBlobStream (Mode : TBlobStreamMode) : TStream;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure FreeBuffers; override;
    function GetAsString: string; override;
    function GetBlobSize: Longint; virtual;
    function GetIsNull: Boolean; override;
    procedure GetText(var TheText: string; ADisplayText: Boolean); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetText(const AValue: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    class function IsBlob: Boolean; override;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure SetFieldType(AValue: TFieldType); override;
    property BlobSize: Longint read FBlobSize;
    property Modified: Boolean read FModified write FModified;
    property Value: string read GetAsString write SetAsString;
    property Transliterate: Boolean read FTransliterate write FTransliterate;
  published
    property BlobType: TBlobType read FBlobType write FBlobType;
    property Size default 0;
  end;

{ TMemoField }

  TMemoField = class(TBlobField)
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Transliterate default True;
  end;

{ TGraphicField }

  TGraphicField = class(TBlobField)
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TIndexDef }

  TIndexDefs = class;

  TIndexOptions = set of (ixPrimary, ixUnique, ixDescending,
    ixCaseInsensitive, ixExpression);

  TIndexDef = class
  Private
    FExpression : String;
    FFields : String;
    FName : String;
    FOptions : TIndexOptions;
    FSource : String;
  public
    constructor Create(Owner: TIndexDefs; const AName, TheFields: string;
      TheOptions: TIndexOptions);
    destructor Destroy; override;
    property Expression: string read FExpression;
    property Fields: string read FFields;
    property Name: string read FName;
    property Options: TIndexOptions read FOptions;
    property Source: string read FSource write FSource;
  end;

{ TIndexDefs }

  TIndexDefs = class
  Private
    FCount : Longint;
    FUpDated : Boolean;
    Function GetItem (Index : longint) : TindexDef;
  public
    constructor Create(DataSet: TDataSet);
    destructor Destroy; override;
    procedure Add(const Name, Fields: string; Options: TIndexOptions);
    procedure Assign(IndexDefs: TIndexDefs);
    procedure Clear;
    function FindIndexForFields(const Fields: string): TIndexDef;
    function GetIndexForFields(const Fields: string;
      CaseInsensitive: Boolean): TIndexDef;
    function IndexOf(const Name: string): Longint;
    procedure Update;
    property Count: Longint read FCount;
    property Items[Index: Longint]: TIndexDef read GetItem; default;
    property Updated: Boolean read FUpdated write FUpdated;
  end;

{ TCheckConstraint }

  TCheckConstraint = class(TCollectionItem)
  Private
    FCustomConstraint : String;
    FErrorMessage : String;
    FFromDictionary : Boolean;
    FImportedConstraint : String;
  public
    procedure Assign(Source: TPersistent); override;
  //  function GetDisplayName: string; override;
  published
    property CustomConstraint: string read FCustomConstraint write FCustomConstraint;
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
    property FromDictionary: Boolean read FFromDictionary write FFromDictionary;
    property ImportedConstraint: string read FImportedConstraint write FImportedConstraint;
  end;

{ TCheckConstraints }

  TCheckConstraints = class(TCollection)
  Private
   Function GetItem(Index : Longint) : TCheckConstraint;
   Procedure SetItem(index : Longint; Value : TCheckConstraint);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Owner: TPersistent);
    function Add: TCheckConstraint;
    property Items[Index: Longint]: TCheckConstraint read GetItem write SetItem; default;
  end;

{ TFields }

  Tfields = Class(TObject)
    Private
      FDataset : TDataset;
      FFieldList : TList;
      FOnChange : TNotifyEvent;
      FValidFieldKinds : TFieldKinds;
    Protected
      Procedure Changed;
      Procedure CheckfieldKind(Fieldkind : TFieldKind; Field : TField);
      Function GetCount : Longint;
      Function GetField (Index : longint) : TField;
      Procedure SetFieldIndex (Field : TField;Value : Integer);
      Property OnChange : TNotifyEvent Read FOnChange Write FOnChange;
      Property ValidFieldKinds : TFieldKinds Read FValidFieldKinds;
    Public
      Constructor Create(ADataset : TDataset);
      Destructor Destroy;override;
      Procedure Add(Field : TField);
      Procedure CheckFieldName (Const Value : String);
      Procedure CheckFieldNames (Const Value : String);
      Procedure Clear;
      Function FindField (Const Value : String) : TField;
      Function FieldByName (Const Value : String) : TField;
      Function FieldByNumber(FieldNo : Integer) : TField;
      Procedure GetFieldNames (Values : TStrings);
      Function IndexOf(Field : TField) : Longint;
      procedure Remove(Value : TField);
      Property Count : Integer Read GetCount;
      Property Dataset : TDataset Read FDataset;
      Property Fields [Index : Integer] : TField Read GetField; default;
    end;


{ TDataSet }

  TBookmark = Pointer;
  TBookmarkStr = string;

  PBookmarkFlag = ^TBookmarkFlag;
  TBookmarkFlag = (bfCurrent, bfBOF, bfEOF, bfInserted);

  PBufferList = ^TBufferList;
  TBufferList = array[0..dsMaxBufferCount - 1] of PChar;

  TGetMode = (gmCurrent, gmNext, gmPrior);

  TGetResult = (grOK, grBOF, grEOF, grError);

  TResyncMode = set of (rmExact, rmCenter);

  TDataAction = (daFail, daAbort, daRetry);

  TUpdateKind = (ukModify, ukInsert, ukDelete);


  TLocateOption = (loCaseInsensitive, loPartialKey);
  TLocateOptions = set of TLocateOption;

  TDataOperation = procedure of object;

  TDataSetNotifyEvent = procedure(DataSet: TDataSet) of object;
  TDataSetErrorEvent = procedure(DataSet: TDataSet; E: EDatabaseError;
    var Action: TDataAction) of object;

  TFilterOption = (foCaseInsensitive, foNoPartialCompare);
  TFilterOptions = set of TFilterOption;

  TFilterRecordEvent = procedure(DataSet: TDataSet;
    var Accept: Boolean) of object;

  TDatasetClass = Class of TDataset;
  TBufferArray = ^pchar;

  TDataSet = class(TComponent)
  Private
    FActive: Boolean;
    FActiveRecord: Longint;
    FAfterCancel: TDataSetNotifyEvent;
    FAfterClose: TDataSetNotifyEvent;
    FAfterDelete: TDataSetNotifyEvent;
    FAfterEdit: TDataSetNotifyEvent;
    FAfterInsert: TDataSetNotifyEvent;
    FAfterOpen: TDataSetNotifyEvent;
    FAfterPost: TDataSetNotifyEvent;
    FAfterScroll: TDataSetNotifyEvent;
    FAutoCalcFields: Boolean;
    FBOF: Boolean;
    FBeforeCancel: TDataSetNotifyEvent;
    FBeforeClose: TDataSetNotifyEvent;
    FBeforeDelete: TDataSetNotifyEvent;
    FBeforeEdit: TDataSetNotifyEvent;
    FBeforeInsert: TDataSetNotifyEvent;
    FBeforeOpen: TDataSetNotifyEvent;
    FBeforePost: TDataSetNotifyEvent;
    FBeforeScroll: TDataSetNotifyEvent;
    FBlobFieldCount: Longint;
    FBookmark: TBookmarkStr;
    FBookmarkSize: Longint;
    FBuffers : TBufferArray;
    FBufferCount: Longint;
    FCalcBuffer: PChar;
    FCalcFieldsSize: Longint;
    FCanModify: Boolean;
    FConstraints: TCheckConstraints;
    FDisableControlsCount : Integer;
    FDisableControlsState : TDatasetState;
    FCurrentRecord: Longint;
    FDataSources : TList;
    FDefaultFields: Boolean;
    FEOF: Boolean;
    FEnableControlsEvent : TDataEvent;
    FFieldList : TFields;
    FFieldCount : Longint;
    FFieldDefs: TFieldDefs;
    FFilterOptions: TFilterOptions;
    FFilterText: string;
    FFiltered: Boolean;
    FFound: Boolean;
    FInternalCalcFields: Boolean;
    FModified: Boolean;
    FOnCalcFields: TDataSetNotifyEvent;
    FOnDeleteError: TDataSetErrorEvent;
    FOnEditError: TDataSetErrorEvent;
    FOnFilterRecord: TFilterRecordEvent;
    FOnNewRecord: TDataSetNotifyEvent;
    FOnPostError: TDataSetErrorEvent;
    FRecNo: Longint;
    FRecordCount: Longint;
    FRecordSize: Word;
    FState : TDataSetState;
    Procedure DoInsertAppend(DoAppend : Boolean);
    Procedure DoInternalOpen;
    Procedure DoInternalClose;
    Function  GetBuffer (Index : longint) : Pchar;
    Function  GetField (Index : Longint) : TField;
    procedure RecalcBufListSize;
    Procedure RegisterDataSource(ADatasource : TDataSource);
    Procedure RemoveField (Field : TField);
    Procedure SetActive (Value : Boolean);
    procedure SetBufferCount(const AValue: Longint);
    Procedure SetField (Index : Longint;Value : TField);
    Procedure ShiftBuffers (Offset,Distance : Longint);
    Function  TryDoing (P : TDataOperation; Ev : TDatasetErrorEvent) : Boolean;
    Procedure UnRegisterDataSource(ADatasource : TDatasource);
    Procedure UpdateFieldDefs;
  protected
    procedure ActivateBuffers; virtual;
    procedure BindFields(Binding: Boolean);
    function  BookmarkAvailable: Boolean;
    procedure CalculateFields(Buffer: PChar); virtual;
    procedure CheckActive; virtual;
    procedure CheckInactive; virtual;
    procedure ClearBuffers; virtual;
    procedure ClearCalcFields(Buffer: PChar); virtual;
    procedure CloseBlob(Field: TField); virtual;
    procedure CloseCursor; virtual;
    procedure CreateFields;
    procedure DataEvent(Event: TDataEvent; Info: Longint); virtual;
    procedure DestroyFields; virtual;
    procedure DoAfterCancel; virtual;
    procedure DoAfterClose; virtual;
    procedure DoAfterDelete; virtual;
    procedure DoAfterEdit; virtual;
    procedure DoAfterInsert; virtual;
    procedure DoAfterOpen; virtual;
    procedure DoAfterPost; virtual;
    procedure DoAfterScroll; virtual;
    procedure DoBeforeCancel; virtual;
    procedure DoBeforeClose; virtual;
    procedure DoBeforeDelete; virtual;
    procedure DoBeforeEdit; virtual;
    procedure DoBeforeInsert; virtual;
    procedure DoBeforeOpen; virtual;
    procedure DoBeforePost; virtual;
    procedure DoBeforeScroll; virtual;
    procedure DoOnCalcFields; virtual;
    procedure DoOnNewRecord; virtual;
    function  FieldByNumber(FieldNo: Longint): TField;
    function  FindRecord(Restart, GoForward: Boolean): Boolean; virtual;
    procedure FreeFieldBuffers; virtual;
    function  GetBookmarkStr: TBookmarkStr; virtual;
    procedure GetCalcFields(Buffer: PChar); virtual;
    function  GetCanModify: Boolean; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function  GetFieldClass(FieldType: TFieldType): TFieldClass; virtual;
    Function  GetfieldCount : Integer;
    function  GetIsIndexField(Field: TField): Boolean; virtual;
    function  GetNextRecords: Longint; virtual;
    function  GetNextRecord: Boolean; virtual;
    function  GetPriorRecords: Longint; virtual;
    function  GetPriorRecord: Boolean; virtual;
    function  GetRecordCount: Longint; virtual;
    function  GetRecNo: Longint; virtual;
    procedure InitFieldDefs; virtual;
    procedure InitRecord(Buffer: PChar); virtual;
    procedure InternalCancel; virtual;
    procedure InternalEdit; virtual;
    procedure InternalRefresh; virtual;
    procedure Loaded; override;
    procedure OpenCursor(InfoQuery: Boolean); virtual;
    procedure RefreshInternalCalcFields(Buffer: PChar); virtual;
    Function  RequiredBuffers : longint;
    procedure RestoreState(const Value: TDataSetState);
    procedure SetBookmarkStr(const Value: TBookmarkStr); virtual;
    procedure SetBufListSize(Value: Longint);
    procedure SetChildOrder(Component: TComponent; Order: Longint); override;
    procedure SetCurrentRecord(Index: Longint); virtual;
    procedure SetFiltered(Value: Boolean); virtual;
    procedure SetFilterOptions(Value: TFilterOptions); virtual;
    procedure SetFilterText(const Value: string); virtual;
    procedure SetFound(const Value: Boolean);
    procedure SetModified(Value: Boolean);
    procedure SetName(const Value: TComponentName); override;
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); virtual;
    procedure SetRecNo(Value: Longint); virtual;
    procedure SetState(Value: TDataSetState);
    function SetTempState(const Value: TDataSetState): TDataSetState;
    function TempBuffer: PChar;
    procedure UpdateIndexDefs; virtual;
    property ActiveRecord: Longint read FActiveRecord;
    property CurrentRecord: Longint read FCurrentRecord;
    property BlobFieldCount: Longint read FBlobFieldCount;
    property BookmarkSize: Longint read FBookmarkSize write FBookmarkSize;
    property Buffers[Index: Longint]: PChar read GetBuffer;
    property BufferCount: Longint read FBufferCount;
    property CalcBuffer: PChar read FCalcBuffer;
    property CalcFieldsSize: Longint read FCalcFieldsSize;
    property InternalCalcFields: Boolean read FInternalCalcFields;
    property Constraints: TCheckConstraints read FConstraints write FConstraints;
  protected { abstract methods }
    function AllocRecordBuffer: PChar; virtual; abstract;
    procedure FreeRecordBuffer(var Buffer: PChar); virtual; abstract;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); virtual; abstract;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; virtual; abstract;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; virtual; abstract;
    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; virtual; abstract;
    function GetRecordSize: Word; virtual; abstract;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); virtual; abstract;
    procedure InternalClose; virtual; abstract;
    procedure InternalDelete; virtual; abstract;
    procedure InternalFirst; virtual; abstract;
    procedure InternalGotoBookmark(ABookmark: Pointer); virtual; abstract;
    procedure InternalHandleException; virtual; abstract;
    procedure InternalInitFieldDefs; virtual; abstract;
    procedure InternalInitRecord(Buffer: PChar); virtual; abstract;
    procedure InternalLast; virtual; abstract;
    procedure InternalOpen; virtual; abstract;
    procedure InternalPost; virtual; abstract;
    procedure InternalSetToRecord(Buffer: PChar); virtual; abstract;
    function IsCursorOpen: Boolean; virtual; abstract;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); virtual; abstract;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); virtual; abstract;
    procedure SetFieldData(Field: TField; Buffer: Pointer); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ActiveBuffer: PChar;
    procedure Append;
    procedure AppendRecord(const Values: array of const);
    function BookmarkValid(ABookmark: TBookmark): Boolean; virtual;
    procedure Cancel; virtual;
    procedure CheckBrowseMode;
    procedure ClearFields;
    procedure Close;
    function  ControlsDisabled: Boolean;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Longint; virtual;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; virtual;
    procedure CursorPosChanged;
    procedure Delete;
    procedure DisableControls;
    procedure Edit;
    procedure EnableControls;
    function FieldByName(const FieldName: string): TField;
    function FindField(const FieldName: string): TField;
    function FindFirst: Boolean;
    function FindLast: Boolean;
    function FindNext: Boolean;
    function FindPrior: Boolean;
    procedure First;
    procedure FreeBookmark(ABookmark: TBookmark); virtual;
    function GetBookmark: TBookmark; virtual;
    function GetCurrentRecord(Buffer: PChar): Boolean; virtual;
    procedure GetFieldList(List: TList; const FieldNames: string);
    procedure GetFieldNames(List: TStrings);
    procedure GotoBookmark(ABookmark: TBookmark);
    procedure Insert;
    procedure InsertRecord(const Values: array of const);
    function IsEmpty: Boolean;
    function IsSequenced: Boolean; virtual;
    procedure Last;
    function MoveBy(Distance: Longint): Longint;
    procedure Next;
    procedure Open;
    procedure Post; virtual;
    procedure Prior;
    procedure Refresh;
    procedure Resync(Mode: TResyncMode); virtual;
    procedure SetFields(const Values: array of const);
    procedure Translate(Src, Dest: PChar; ToOem: Boolean); virtual;
    procedure UpdateCursorPos;
    procedure UpdateRecord;
    property BOF: Boolean read FBOF;
    property Bookmark: TBookmarkStr read GetBookmarkStr write SetBookmarkStr;
    property CanModify: Boolean read GetCanModify;
    property DefaultFields: Boolean read FDefaultFields;
    property EOF: Boolean read FEOF;
    property FieldCount: Longint read GetFieldCount;
    property FieldDefs: TFieldDefs read FFieldDefs write FFieldDefs;
    property Fields[Index: Longint]: TField read GetField write SetField;
    property Found: Boolean read FFound;
    property Modified: Boolean read FModified;
    property RecordCount: Longint read GetRecordCount;
    property RecNo: Longint read FRecNo write FRecNo;
    property RecordSize: Word read FRecordSize;
    property State: TDataSetState read FState;
//    property Fields : TFields Read FFieldList;
    property Filter: string read FFilterText write FFilterText;
    property Filtered: Boolean read FFiltered write FFiltered default False;
    property FilterOptions: TFilterOptions read FFilterOptions write FFilterOptions;
    property Active: Boolean read FActive write SetActive default False;
    property AutoCalcFields: Boolean read FAutoCalcFields write FAutoCalcFields;
    property BeforeOpen: TDataSetNotifyEvent read FBeforeOpen write FBeforeOpen;
    property AfterOpen: TDataSetNotifyEvent read FAfterOpen write FAfterOpen;
    property BeforeClose: TDataSetNotifyEvent read FBeforeClose write FBeforeClose;
    property AfterClose: TDataSetNotifyEvent read FAfterClose write FAfterClose;
    property BeforeInsert: TDataSetNotifyEvent read FBeforeInsert write FBeforeInsert;
    property AfterInsert: TDataSetNotifyEvent read FAfterInsert write FAfterInsert;
    property BeforeEdit: TDataSetNotifyEvent read FBeforeEdit write FBeforeEdit;
    property AfterEdit: TDataSetNotifyEvent read FAfterEdit write FAfterEdit;
    property BeforePost: TDataSetNotifyEvent read FBeforePost write FBeforePost;
    property AfterPost: TDataSetNotifyEvent read FAfterPost write FAfterPost;
    property BeforeCancel: TDataSetNotifyEvent read FBeforeCancel write FBeforeCancel;
    property AfterCancel: TDataSetNotifyEvent read FAfterCancel write FAfterCancel;
    property BeforeDelete: TDataSetNotifyEvent read FBeforeDelete write FBeforeDelete;
    property AfterDelete: TDataSetNotifyEvent read FAfterDelete write FAfterDelete;
    property BeforeScroll: TDataSetNotifyEvent read FBeforeScroll write FBeforeScroll;
    property AfterScroll: TDataSetNotifyEvent read FAfterScroll write FAfterScroll;
    property OnCalcFields: TDataSetNotifyEvent read FOnCalcFields write FOnCalcFields;
    property OnDeleteError: TDataSetErrorEvent read FOnDeleteError write FOnDeleteError;
    property OnEditError: TDataSetErrorEvent read FOnEditError write FOnEditError;
    property OnFilterRecord: TFilterRecordEvent read FOnFilterRecord write SetOnFilterRecord;
    property OnNewRecord: TDataSetNotifyEvent read FOnNewRecord write FOnNewRecord;
    property OnPostError: TDataSetErrorEvent read FOnPostError write FOnPostError;
  end;

  TDataLink = class(TPersistent)
  private
    FFIrstRecord,
    FBufferCount : Integer;
    FActive,
    FDataSourceFixed,
    FEditing,
    FReadOnly,
    FUpdatingRecord,
    FVisualControl : Boolean;
    FDataSource : TDataSource;
    Function  CalcFirstRecord(Index : Integer) : Integer;
    Procedure CalcRange;
    Procedure CheckActiveAndEditing;
    Function  GetDataset : TDataset;
    procedure SetActive(AActive: Boolean);
    procedure SetDataSource(Value: TDataSource);
    Procedure SetReadOnly(Value : Boolean);
  protected
    procedure ActiveChanged; virtual;
    procedure CheckBrowseMode; virtual;
    procedure DataEvent(Event: TDataEvent; Info: Longint); virtual;
    procedure DataSetChanged; virtual;
    procedure DataSetScrolled(Distance: Integer); virtual;
    procedure EditingChanged; virtual;
    procedure FocusControl(Field: TFieldRef); virtual;
    function  GetActiveRecord: Integer; virtual;
    function  GetBOF: Boolean; virtual;
    function  GetBufferCount: Integer; virtual;
    function  GetEOF: Boolean; virtual;
    function  GetRecordCount: Integer; virtual;
    procedure LayoutChanged; virtual;
    function  MoveBy(Distance: Integer): Integer; virtual;
    procedure RecordChanged(Field: TField); virtual;
    procedure SetActiveRecord(Value: Integer); virtual;
    procedure SetBufferCount(Value: Integer); virtual;
    procedure UpdateData; virtual;
    property VisualControl: Boolean read FVisualControl write FVisualControl;
  public
    constructor Create;
    destructor Destroy; override;
    function  Edit: Boolean;
    procedure UpdateRecord;
    property Active: Boolean read FActive;
    property ActiveRecord: Integer read GetActiveRecord write SetActiveRecord;
    property BOF: Boolean read GetBOF;
    property BufferCount: Integer read FBufferCount write SetBufferCount;
    property DataSet: TDataSet read GetDataSet;
    property DataSource: TDataSource read FDataSource write SetDataSource;
    property DataSourceFixed: Boolean read FDataSourceFixed write FDataSourceFixed;
    property Editing: Boolean read FEditing;
    property Eof: Boolean read GetEOF;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property RecordCount: Integer read GetRecordCount;
  end;

{ TDetailDataLink }

  TDetailDataLink = class(TDataLink)
  protected
    function GetDetailDataSet: TDataSet; virtual;
  public
    property DetailDataSet: TDataSet read GetDetailDataSet;
  end;

{ TMasterDataLink }

  TMasterDataLink = class(TDetailDataLink)
  private
    FDataSet: TDataSet;
    FFieldNames: string;
    FFields: TList;
    FOnMasterChange: TNotifyEvent;
    FOnMasterDisable: TNotifyEvent;
    procedure SetFieldNames(const Value: string);
  protected
    procedure ActiveChanged; override;
    procedure CheckBrowseMode; override;
    function GetDetailDataSet: TDataSet; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create(ADataSet: TDataSet);
    destructor Destroy; override;
    property FieldNames: string read FFieldNames write SetFieldNames;
    property Fields: TList read FFields;
    property OnMasterChange: TNotifyEvent read FOnMasterChange write FOnMasterChange;
    property OnMasterDisable: TNotifyEvent read FOnMasterDisable write FOnMasterDisable;
  end;

{ TDataSource }

  TDataChangeEvent = procedure(Sender: TObject; Field: TField) of object;

  TDataSource = class(TComponent)
  private
    FDataSet: TDataSet;
    FDataLinks: TList;
    FEnabled: Boolean;
    FAutoEdit: Boolean;
    FState: TDataSetState;
    FOnStateChange: TNotifyEvent;
    FOnDataChange: TDataChangeEvent;
    FOnUpdateData: TNotifyEvent;
    procedure DistributeEvent(Event: TDataEvent; Info: Longint);
    procedure RegisterDataLink(DataLink: TDataLink);
    Procedure ProcessEvent(Event : TDataEvent; Info : longint);
    procedure SetDataSet(ADataSet: TDataSet);
    procedure SetEnabled(Value: Boolean);
    procedure UnregisterDataLink(DataLink: TDataLink);
  protected
    Procedure DoDataChange (Info : Pointer);virtual;
    Procedure DoStateChange; virtual;
    Procedure DoUpdateData;
    property DataLinks: TList read FDataLinks;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Edit;
    function IsLinkedTo(ADataSet: TDataSet): Boolean;
    property State: TDataSetState read FState;
  published
    property AutoEdit: Boolean read FAutoEdit write FAutoEdit default True;
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property OnDataChange: TDataChangeEvent read FOnDataChange write FOnDataChange;
    property OnUpdateData: TNotifyEvent read FOnUpdateData write FOnUpdateData;
  end;


 { TDBDataset }

  TDBDatasetClass = Class of TDBDataset;
  TDBDataset = Class(TDataset)
    Private
      FDatabase : TDatabase;
      Procedure SetDatabase (Value : TDatabase);
    Protected 
      Procedure CheckDatabase;  
    Public
      Destructor destroy; override;
      Property DataBase : TDatabase Read FDatabase Write SetDatabase;
    end;

  { TDatabase }

  TLoginEvent = procedure(Database: TDatabase;
    LoginParams: TStrings) of object;

  TDatabaseClass = Class Of TDatabase;

  TDatabase = class(TComponent)
  private
    FConnected : Boolean;
    FDataBaseName : String;
    FDataSets : TList;
    FDirectory : String;
    FKeepConnection : Boolean;
    FLoginPrompt : Boolean;
    FOnLogin : TLoginEvent;
    FParams : TStrings;
    FSQLBased : Boolean;
    Function GetDataSetCount : Longint;
    Function GetDataset(Index : longint) : TDBDataset;
    procedure SetConnected (Value : boolean);
    procedure RegisterDataset (DS : TDBDataset);
    procedure UnRegisterDataset (DS : TDBDataset);
    procedure RemoveDataSets;
  protected
    Procedure CheckConnected;
    Procedure CheckDisConnected;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Procedure DoInternalConnect; Virtual;Abstract;
    Procedure DoInternalDisConnect; Virtual;Abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Close;
    procedure Open;
    procedure CloseDataSets;
    procedure StartTransaction; virtual; abstract;
    procedure EndTransaction; virtual; abstract;
    property DataSetCount: Longint read GetDataSetCount;
    property DataSets[Index: Longint]: TDBDataSet read GetDataSet;
    property Directory: string read FDirectory write FDirectory;
    property IsSQLBased: Boolean read FSQLBased;
  published
    property Connected: Boolean read FConnected write SetConnected;
    property DatabaseName: string read FDatabaseName write FDatabaseName;
    property KeepConnection: Boolean read FKeepConnection write FKeepConnection;
    property LoginPrompt: Boolean read FLoginPrompt write FLoginPrompt;
    property Params : TStrings read FParams Write FParams;
    property OnLogin: TLoginEvent read FOnLogin write FOnLogin;
  end;

Const
  Fieldtypenames : Array [TFieldType] of String[15] =
    (
      'Unknown',
      'String',
      'Smallint',
      'Integer',
      'Word',
      'Boolean',
      'Float',
      'Currency',
      'BCD',
      'Date',
      'Time',
      'DateTime',
      'Bytes',
      'VarBytes',
      'AutoInc',
      'Blob',
      'Memo',
      'Graphic',
      'FmtMemo',
      'ParadoxOle',
      'DBaseOle',
      'TypedBinary',
      'Cursor',
      'FixedChar',
      'WideString',
      'Largeint',
      'ADT',
      'Array',
      'Reference',
      'DataSet',
      'OraBlob',
      'OraClob',
      'Variant',
      'Interface',
      'IDispatch',
      'Guid',
      'TimeStamp',
      'FMTBcd'
    );
    { 'Unknown',
      'String',
      'Smallint',
      'Integer',
      'Word',
      'Boolean',
      'Float',
      'Date',
      'Time',
      'DateTime',
      'Bytes',
      'VarBytes',
      'AutoInc',
      'Blob',
      'Memo',
      'Graphic',
      'FmtMemo',
      'ParadoxOle',
      'DBaseOle',
      'TypedBinary',
      'Cursor'
    );}

   dsEditModes = [dsEdit, dsInsert];
{ Auxiliary functions }

Procedure DatabaseError (Const Msg : String);
Procedure DatabaseError (Const Msg : String; Comp : TComponent);
Procedure DatabaseErrorFmt (Const Fmt : String; Args : Array Of Const);
Procedure DatabaseErrorFmt (Const Fmt : String; Args : Array Of const;
                            Comp : TComponent);

implementation

{ ---------------------------------------------------------------------
    Auxiliary functions
  ---------------------------------------------------------------------}



Procedure DatabaseError (Const Msg : String);

begin
  Raise EDataBaseError.Create(Msg);
end;

Procedure DatabaseError (Const Msg : String; Comp : TComponent);

begin
  Raise EDatabaseError.CreateFmt('%s : %s',[Comp.Name,Msg]);
end;

Procedure DatabaseErrorFmt (Const Fmt : String; Args : Array Of Const);

begin
  Raise EDatabaseError.CreateFmt(Fmt,Args);
end;

Procedure DatabaseErrorFmt (Const Fmt : String; Args : Array Of const;
                            Comp : TComponent);
begin
  Raise EDatabaseError.CreateFmt(Format('%s : %s',[Comp.Name,Fmt]),Args);
end;


{$i dbs.inc}

{ TIndexDef }

constructor TIndexDef.Create(Owner: TIndexDefs; const AName, TheFields: string;
      TheOptions: TIndexOptions);

begin
  //!! To be implemented
end;



destructor TIndexDef.Destroy;

begin
  //!! To be implemented
end;


{ TIndexDefs }

Function TIndexDefs.GetItem (Index : longint) : TindexDef;

begin
  //!! To be implemented
end;


constructor TIndexDefs.Create(DataSet: TDataSet);

begin
  //!! To be implemented
end;


destructor TIndexDefs.Destroy;

begin
  //!! To be implemented
end;


procedure TIndexDefs.Add(const Name, Fields: string; Options: TIndexOptions);

begin
  //!! To be implemented
end;


procedure TIndexDefs.Assign(IndexDefs: TIndexDefs);

begin
  //!! To be implemented
end;


procedure TIndexDefs.Clear;

begin
  //!! To be implemented
end;


function TIndexDefs.FindIndexForFields(const Fields: string): TIndexDef;

begin
  //!! To be implemented
end;


function TIndexDefs.GetIndexForFields(const Fields: string;
  CaseInsensitive: Boolean): TIndexDef;

begin
  //!! To be implemented
end;


function TIndexDefs.IndexOf(const Name: string): Longint;

begin
  //!! To be implemented
end;


procedure TIndexDefs.Update;

begin
  //!! To be implemented
end;



{ TCheckConstraint }

procedure TCheckConstraint.Assign(Source: TPersistent);

begin
  //!! To be implemented
end;



{ TCheckConstraints }

Function TCheckConstraints.GetItem(Index : Longint) : TCheckConstraint;

begin
  //!! To be implemented
end;


Procedure TCheckConstraints.SetItem(index : Longint; Value : TCheckConstraint);

begin
  //!! To be implemented
end;


function TCheckConstraints.GetOwner: TPersistent;

begin
  //!! To be implemented
end;


constructor TCheckConstraints.Create(Owner: TPersistent);

begin
  //!! To be implemented
end;


function TCheckConstraints.Add: TCheckConstraint;

begin
  //!! To be implemented
end;



{$i dataset.inc}
{$i fields.inc}
{$i datasource.inc}
{$i database.inc}

end.

{
  $Log$
  Revision 1.10  2003-08-16 16:42:21  michael
  + Fixes in TDBDataset etc. Changed MySQLDb to use database as well

  Revision 1.9  2003/05/15 15:15:15  michael
  + Database class in TDBDataset is public, not published

  Revision 1.8  2003/05/08 21:52:41  michael
  + Patch from Jesus Reyes

  Revision 1.7  2003/02/20 19:30:28  michael
  + Fixes from Jesus Reyes

  Revision 1.6  2002/09/07 15:15:23  peter
    * old logs removed and tabs fixed

}
