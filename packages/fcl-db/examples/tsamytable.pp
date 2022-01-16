Unit tsamytable;

{$mode objfpc}{$H+}

Interface

Uses Classes, SysUtils, db, fieldmap, sqldb;

Const
  IID_MyTypeSafeAccess = '{1258E169-56C8-4846-8BAF-928C06B89487}';

  // Field names
  FLD_MyTypeSafeAccess_MyString = 'MyString';
  FLD_MyTypeSafeAccess_MyFixedChar = 'MyFixedChar';
  FLD_MyTypeSafeAccess_MyWideString = 'MyWideString';
  FLD_MyTypeSafeAccess_MyDateTime = 'MyDateTime';
  FLD_MyTypeSafeAccess_MyUnicodeString = 'MyUnicodeString';
  FLD_MyTypeSafeAccess_MyUTF8String = 'MyUTF8String';
  FLD_MyTypeSafeAccess_MyFixedWideString = 'MyFixedWideString';
  FLD_MyTypeSafeAccess_MyInteger = 'MyInteger';
  FLD_MyTypeSafeAccess_MyByteInteger = 'MyByteInteger';
  FLD_MyTypeSafeAccess_MySmallintInteger = 'MySmallintInteger';
  FLD_MyTypeSafeAccess_MyShortIntInteger = 'MyShortIntInteger';
  FLD_MyTypeSafeAccess_MyCardinalInteger = 'MyCardinalInteger';
  FLD_MyTypeSafeAccess_MyFloat = 'MyFloat';
  FLD_MyTypeSafeAccess_MyWord = 'MyWord';
  FLD_MyTypeSafeAccess_MyBoolean = 'MyBoolean';
  FLD_MyTypeSafeAccess_MyInt64 = 'MyInt64';
  FLD_MyTypeSafeAccess_MyQWordLargeInt = 'MyQWordLargeInt';
  FLD_MyTypeSafeAccess_MyBlob = 'MyBlob';

  SQLMyTypeSafeAccess = 
    'SELECT' + sLineBreak +
    'MyString' + sLineBreak +
    ', MyFixedChar' + sLineBreak +
    ', MyWideString' + sLineBreak +
    ', MyDateTime' + sLineBreak +
    ', MyUnicodeString' + sLineBreak +
    ', MyUTF8String' + sLineBreak +
    ', MyFixedWideString' + sLineBreak +
    ', MyInteger' + sLineBreak +
    ', MyByteInteger' + sLineBreak +
    ', MySmallintInteger' + sLineBreak +
    ', MyShortIntInteger' + sLineBreak +
    ', MyCardinalInteger' + sLineBreak +
    ', MyFloat' + sLineBreak +
    ', MyWord' + sLineBreak +
    ', MyBoolean' + sLineBreak +
    ', MyInt64' + sLineBreak +
    ', MyQWordLargeInt' + sLineBreak +
    ', MyBlob' + sLineBreak +
    'FROM MyTable';


Type

{$INLINE ON}

  { IMyTypeSafeAccess }

  IMyTypeSafeAccess = Interface(ITypeSafeDatasetAccess) [IID_MyTypeSafeAccess]
    Function GetMyString : AnsiString;
    Procedure SetMyString (aValue : AnsiString);
    Function GetMyFixedChar : AnsiString;
    Procedure SetMyFixedChar (aValue : AnsiString);
    Function GetMyWideString : WideString;
    Procedure SetMyWideString (aValue : WideString);
    Function GetMyDateTime : TDateTime;
    Procedure SetMyDateTime (aValue : TDateTime);
    Function GetMyUnicodeString : UnicodeString;
    Procedure SetMyUnicodeString (aValue : UnicodeString);
    Function GetMyUTF8String : Utf8String;
    Procedure SetMyUTF8String (aValue : Utf8String);
    Function GetMyFixedWideString : WideString;
    Procedure SetMyFixedWideString (aValue : WideString);
    Function GetMyInteger : Longint;
    Procedure SetMyInteger (aValue : Longint);
    Function GetMyByteInteger : Byte;
    Procedure SetMyByteInteger (aValue : Byte);
    Function GetMySmallintInteger : SmallInt;
    Procedure SetMySmallintInteger (aValue : SmallInt);
    Function GetMyShortIntInteger : ShortInt;
    Procedure SetMyShortIntInteger (aValue : ShortInt);
    Function GetMyCardinalInteger : Cardinal;
    Procedure SetMyCardinalInteger (aValue : Cardinal);
    Function GetMyFloat : Double;
    Procedure SetMyFloat (aValue : Double);
    Function GetMyWord : Word;
    Procedure SetMyWord (aValue : Word);
    Function GetMyBoolean : Boolean;
    Procedure SetMyBoolean (aValue : Boolean);
    Function GetMyInt64 : Int64;
    Procedure SetMyInt64 (aValue : Int64);
    Function GetMyQWordLargeInt : QWord;
    Procedure SetMyQWordLargeInt (aValue : QWord);
    Function GetMyBlob : TStream;
    Property MyString : AnsiString Read GetMyString Write SetMyString;
    Property MyFixedChar : AnsiString Read GetMyFixedChar Write SetMyFixedChar;
    Property MyWideString : WideString Read GetMyWideString Write SetMyWideString;
    Property MyDateTime : TDateTime Read GetMyDateTime Write SetMyDateTime;
    Property MyUnicodeString : UnicodeString Read GetMyUnicodeString Write SetMyUnicodeString;
    Property MyUTF8String : Utf8String Read GetMyUTF8String Write SetMyUTF8String;
    Property MyFixedWideString : WideString Read GetMyFixedWideString Write SetMyFixedWideString;
    Property MyInteger : Longint Read GetMyInteger Write SetMyInteger;
    Property MyByteInteger : Byte Read GetMyByteInteger Write SetMyByteInteger;
    Property MySmallintInteger : SmallInt Read GetMySmallintInteger Write SetMySmallintInteger;
    Property MyShortIntInteger : ShortInt Read GetMyShortIntInteger Write SetMyShortIntInteger;
    Property MyCardinalInteger : Cardinal Read GetMyCardinalInteger Write SetMyCardinalInteger;
    Property MyFloat : Double Read GetMyFloat Write SetMyFloat;
    Property MyWord : Word Read GetMyWord Write SetMyWord;
    Property MyBoolean : Boolean Read GetMyBoolean Write SetMyBoolean;
    Property MyInt64 : Int64 Read GetMyInt64 Write SetMyInt64;
    Property MyQWordLargeInt : QWord Read GetMyQWordLargeInt Write SetMyQWordLargeInt;
    Property MyBlob : TStream Read GetMyBlob;
  end;
  { TMyTypeSafeAccess }

  TMyTypeSafeAccess = Class(TTypeSafeDatasetAccess,IMyTypeSafeAccess)
  Private
    FBlobMyBlob : TBlobProxyStream;
    Procedure DoMyBlobChanged(Sender : TObject);
  Private
    Function GetMyString : AnsiString;
    Procedure SetMyString (AValue  : AnsiString);
    Function GetMyFixedChar : AnsiString;
    Procedure SetMyFixedChar (AValue  : AnsiString);
    Function GetMyWideString : WideString;
    Procedure SetMyWideString (AValue  : WideString);
    Function GetMyDateTime : TDateTime;
    Procedure SetMyDateTime (AValue  : TDateTime);
    Function GetMyUnicodeString : UnicodeString;
    Procedure SetMyUnicodeString (AValue  : UnicodeString);
    Function GetMyUTF8String : Utf8String;
    Procedure SetMyUTF8String (AValue  : Utf8String);
    Function GetMyFixedWideString : WideString;
    Procedure SetMyFixedWideString (AValue  : WideString);
    Function GetMyInteger : Longint;
    Procedure SetMyInteger (AValue  : Longint);
    Function GetMyByteInteger : Byte;
    Procedure SetMyByteInteger (AValue  : Byte);
    Function GetMySmallintInteger : SmallInt;
    Procedure SetMySmallintInteger (AValue  : SmallInt);
    Function GetMyShortIntInteger : ShortInt;
    Procedure SetMyShortIntInteger (AValue  : ShortInt);
    Function GetMyCardinalInteger : Cardinal;
    Procedure SetMyCardinalInteger (AValue  : Cardinal);
    Function GetMyFloat : Double;
    Procedure SetMyFloat (AValue  : Double);
    Function GetMyWord : Word;
    Procedure SetMyWord (AValue  : Word);
    Function GetMyBoolean : Boolean;
    Procedure SetMyBoolean (AValue  : Boolean);
    Function GetMyInt64 : Int64;
    Procedure SetMyInt64 (AValue  : Int64);
    Function GetMyQWordLargeInt : QWord;
    Procedure SetMyQWordLargeInt (AValue  : QWord);
    Function GetMyBlob : TStream;
  Protected
    Class Function FieldMapClass : TFieldMapClass; override;
  Public
    Destructor Destroy; Override;
    Procedure ApplyUpdates; override;
    Class Function CreateQuery(aSQL : String; aConnection : TSQLConnection; aTransaction : TSQLTransaction) : TMyTypeSafeAccess; overload;
    Class Function CreateQuery(aConnection : TSQLConnection; aTransaction : TSQLTransaction) : TMyTypeSafeAccess; overload;
    Class Function GetQuery(aSQL : String; aConnection : TSQLConnection; aTransaction : TSQLTransaction) : IMyTypeSafeAccess; overload;
    Class Function GetQuery(aConnection : TSQLConnection; aTransaction : TSQLTransaction) : IMyTypeSafeAccess; overload;
  Published
    Property MyString : AnsiString Read GetMyString Write SetMyString;
    Property MyFixedChar : AnsiString Read GetMyFixedChar Write SetMyFixedChar;
    Property MyWideString : WideString Read GetMyWideString Write SetMyWideString;
    Property MyDateTime : TDateTime Read GetMyDateTime Write SetMyDateTime;
    Property MyUnicodeString : UnicodeString Read GetMyUnicodeString Write SetMyUnicodeString;
    Property MyUTF8String : Utf8String Read GetMyUTF8String Write SetMyUTF8String;
    Property MyFixedWideString : WideString Read GetMyFixedWideString Write SetMyFixedWideString;
    Property MyInteger : Longint Read GetMyInteger Write SetMyInteger;
    Property MyByteInteger : Byte Read GetMyByteInteger Write SetMyByteInteger;
    Property MySmallintInteger : SmallInt Read GetMySmallintInteger Write SetMySmallintInteger;
    Property MyShortIntInteger : ShortInt Read GetMyShortIntInteger Write SetMyShortIntInteger;
    Property MyCardinalInteger : Cardinal Read GetMyCardinalInteger Write SetMyCardinalInteger;
    Property MyFloat : Double Read GetMyFloat Write SetMyFloat;
    Property MyWord : Word Read GetMyWord Write SetMyWord;
    Property MyBoolean : Boolean Read GetMyBoolean Write SetMyBoolean;
    Property MyInt64 : Int64 Read GetMyInt64 Write SetMyInt64;
    Property MyQWordLargeInt : QWord Read GetMyQWordLargeInt Write SetMyQWordLargeInt;
    Property MyBlob : TStream Read GetMyBlob;
  end;


Implementation

Type

  { TMyFieldMap }

  TMyFieldMap = Class(TFieldMap)
  Private
    FMyString : TField;
    FMyFixedChar : TField;
    FMyWideString : TField;
    FMyDateTime : TField;
    FMyUnicodeString : TField;
    FMyUTF8String : TField;
    FMyFixedWideString : TField;
    FMyInteger : TField;
    FMyByteInteger : TField;
    FMySmallintInteger : TField;
    FMyShortIntInteger : TField;
    FMyCardinalInteger : TField;
    FMyFloat : TField;
    FMyWord : TField;
    FMyBoolean : TField;
    FMyInt64 : TField;
    FMyQWordLargeInt : TField;
    FMyBlob : TField;
  Public
    Procedure InitFields; Override;
    Property MyString : TField read FMyString;
    Property MyFixedChar : TField read FMyFixedChar;
    Property MyWideString : TField read FMyWideString;
    Property MyDateTime : TField read FMyDateTime;
    Property MyUnicodeString : TField read FMyUnicodeString;
    Property MyUTF8String : TField read FMyUTF8String;
    Property MyFixedWideString : TField read FMyFixedWideString;
    Property MyInteger : TField read FMyInteger;
    Property MyByteInteger : TField read FMyByteInteger;
    Property MySmallintInteger : TField read FMySmallintInteger;
    Property MyShortIntInteger : TField read FMyShortIntInteger;
    Property MyCardinalInteger : TField read FMyCardinalInteger;
    Property MyFloat : TField read FMyFloat;
    Property MyWord : TField read FMyWord;
    Property MyBoolean : TField read FMyBoolean;
    Property MyInt64 : TField read FMyInt64;
    Property MyQWordLargeInt : TField read FMyQWordLargeInt;
    Property MyBlob : TField read FMyBlob;
  end;
 { TMyTypeSafeAccess } 

 { Constructor and destructor }

Destructor TMyTypeSafeAccess.Destroy;

begin
  FreeAndNil(FBlobMyBlob);
  Inherited;
end;


 { Property Getters }

Function TMyTypeSafeAccess.GetMyString : AnsiString;

begin
  Result:=TMyFieldMap(FieldMap).MyString.AsAnsiString;
end;


Function TMyTypeSafeAccess.GetMyFixedChar : AnsiString;

begin
  Result:=TMyFieldMap(FieldMap).MyFixedChar.AsAnsiString;
end;


Function TMyTypeSafeAccess.GetMyWideString : WideString;

begin
  Result:=TMyFieldMap(FieldMap).MyWideString.AsWideString;
end;


Function TMyTypeSafeAccess.GetMyDateTime : TDateTime;

begin
  Result:=TMyFieldMap(FieldMap).MyDateTime.AsDateTime;
end;


Function TMyTypeSafeAccess.GetMyUnicodeString : UnicodeString;

begin
  Result:=TMyFieldMap(FieldMap).MyUnicodeString.AsUnicodeString;
end;


Function TMyTypeSafeAccess.GetMyUTF8String : Utf8String;

begin
  Result:=TMyFieldMap(FieldMap).MyUTF8String.AsUtf8String;
end;


Function TMyTypeSafeAccess.GetMyFixedWideString : WideString;

begin
  Result:=TMyFieldMap(FieldMap).MyFixedWideString.AsWideString;
end;


Function TMyTypeSafeAccess.GetMyInteger : Longint;

begin
  Result:=TMyFieldMap(FieldMap).MyInteger.AsInteger;
end;


Function TMyTypeSafeAccess.GetMyByteInteger : Byte;

begin
  Result:=TMyFieldMap(FieldMap).MyByteInteger.AsInteger;
end;


Function TMyTypeSafeAccess.GetMySmallintInteger : SmallInt;

begin
  Result:=TMyFieldMap(FieldMap).MySmallintInteger.AsInteger;
end;


Function TMyTypeSafeAccess.GetMyShortIntInteger : ShortInt;

begin
  Result:=TMyFieldMap(FieldMap).MyShortIntInteger.AsInteger;
end;


Function TMyTypeSafeAccess.GetMyCardinalInteger : Cardinal;

begin
  Result:=TMyFieldMap(FieldMap).MyCardinalInteger.AsInteger;
end;


Function TMyTypeSafeAccess.GetMyFloat : Double;

begin
  Result:=TMyFieldMap(FieldMap).MyFloat.AsFLoat;
end;


Function TMyTypeSafeAccess.GetMyWord : Word;

begin
  Result:=TMyFieldMap(FieldMap).MyWord.AsInteger;
end;


Function TMyTypeSafeAccess.GetMyBoolean : Boolean;

begin
  Result:=TMyFieldMap(FieldMap).MyBoolean.AsBoolean;
end;


Function TMyTypeSafeAccess.GetMyInt64 : Int64;

begin
  Result:=TMyFieldMap(FieldMap).MyInt64.AsLargeInt;
end;


Function TMyTypeSafeAccess.GetMyQWordLargeInt : QWord;

begin
  Result:=TMyFieldMap(FieldMap).MyQWordLargeInt.AsLargeInt;
end;


Procedure TMyTypeSafeAccess.DoMyBlobChanged(Sender : TObject);

begin
  If Dataset.State in dsEditModes then
    TBlobField(TMyFieldMap(FieldMap).MyBlob).LoadFromStream(TStream(Sender));
end;


Function TMyTypeSafeAccess.GetMyBlob : TStream;

begin
  if not Assigned(FBlobMyBlob) then
    begin
    FBlobMyBlob:=TBlobProxyStream.Create;
    FBlobMyBlob.OnChange:=@DoMyBlobChanged;
    end;
  FBlobMyBlob.Size:=0;
  FBlobMyBlob.Position:=0;
  if not FBlobMyBlob.Updating then
    begin
    TBlobField(TMyFieldMap(FieldMap).MyBlob).SaveToStream(FBlobMyBlob);
    FBlobMyBlob.Position:=0;
    end;
  Result:=FBlobMyBlob;
end;


 { Property Setters }

Procedure TMyTypeSafeAccess.SetMyString (AValue  : AnsiString);

begin
  TMyFieldMap(FieldMap).MyString.AsAnsiString:=aValue;
end;


Procedure TMyTypeSafeAccess.SetMyFixedChar (AValue  : AnsiString);

begin
  TMyFieldMap(FieldMap).MyFixedChar.AsAnsiString:=aValue;
end;


Procedure TMyTypeSafeAccess.SetMyWideString (AValue  : WideString);

begin
  TMyFieldMap(FieldMap).MyWideString.AsWideString:=aValue;
end;


Procedure TMyTypeSafeAccess.SetMyDateTime (AValue  : TDateTime);

begin
  TMyFieldMap(FieldMap).MyDateTime.AsDateTime:=aValue;
end;


Procedure TMyTypeSafeAccess.SetMyUnicodeString (AValue  : UnicodeString);

begin
  TMyFieldMap(FieldMap).MyUnicodeString.AsUnicodeString:=aValue;
end;


Procedure TMyTypeSafeAccess.SetMyUTF8String (AValue  : Utf8String);

begin
  TMyFieldMap(FieldMap).MyUTF8String.AsUtf8String:=aValue;
end;


Procedure TMyTypeSafeAccess.SetMyFixedWideString (AValue  : WideString);

begin
  TMyFieldMap(FieldMap).MyFixedWideString.AsWideString:=aValue;
end;


Procedure TMyTypeSafeAccess.SetMyInteger (AValue  : Longint);

begin
  TMyFieldMap(FieldMap).MyInteger.AsInteger:=aValue;
end;


Procedure TMyTypeSafeAccess.SetMyByteInteger (AValue  : Byte);

begin
  TMyFieldMap(FieldMap).MyByteInteger.AsInteger:=aValue;
end;


Procedure TMyTypeSafeAccess.SetMySmallintInteger (AValue  : SmallInt);

begin
  TMyFieldMap(FieldMap).MySmallintInteger.AsInteger:=aValue;
end;


Procedure TMyTypeSafeAccess.SetMyShortIntInteger (AValue  : ShortInt);

begin
  TMyFieldMap(FieldMap).MyShortIntInteger.AsInteger:=aValue;
end;


Procedure TMyTypeSafeAccess.SetMyCardinalInteger (AValue  : Cardinal);

begin
  TMyFieldMap(FieldMap).MyCardinalInteger.AsInteger:=aValue;
end;


Procedure TMyTypeSafeAccess.SetMyFloat (AValue  : Double);

begin
  TMyFieldMap(FieldMap).MyFloat.AsFLoat:=aValue;
end;


Procedure TMyTypeSafeAccess.SetMyWord (AValue  : Word);

begin
  TMyFieldMap(FieldMap).MyWord.AsInteger:=aValue;
end;


Procedure TMyTypeSafeAccess.SetMyBoolean (AValue  : Boolean);

begin
  TMyFieldMap(FieldMap).MyBoolean.AsBoolean:=aValue;
end;


Procedure TMyTypeSafeAccess.SetMyInt64 (AValue  : Int64);

begin
  TMyFieldMap(FieldMap).MyInt64.AsLargeInt:=aValue;
end;


Procedure TMyTypeSafeAccess.SetMyQWordLargeInt (AValue  : QWord);

begin
  TMyFieldMap(FieldMap).MyQWordLargeInt.AsLargeInt:=aValue;
end;


Class Function TMyTypeSafeAccess.FieldMapClass : TFieldMapClass;

begin
  Result:=TMyFieldMap;
end;


 { TMyFieldMap }

Procedure TMyFieldMap.InitFields;

begin
  FMyString:=FieldByName('MyString');
  FMyFixedChar:=FieldByName('MyFixedChar');
  FMyWideString:=FieldByName('MyWideString');
  FMyDateTime:=FieldByName('MyDateTime');
  FMyUnicodeString:=FieldByName('MyUnicodeString');
  FMyUTF8String:=FieldByName('MyUTF8String');
  FMyFixedWideString:=FieldByName('MyFixedWideString');
  FMyInteger:=FieldByName('MyInteger');
  FMyByteInteger:=FieldByName('MyByteInteger');
  FMySmallintInteger:=FieldByName('MySmallintInteger');
  FMyShortIntInteger:=FieldByName('MyShortIntInteger');
  FMyCardinalInteger:=FieldByName('MyCardinalInteger');
  FMyFloat:=FieldByName('MyFloat');
  FMyWord:=FieldByName('MyWord');
  FMyBoolean:=FieldByName('MyBoolean');
  FMyInt64:=FieldByName('MyInt64');
  FMyQWordLargeInt:=FieldByName('MyQWordLargeInt');
  FMyBlob:=FieldByName('MyBlob');
end;


Class Function TMyTypeSafeAccess.CreateQuery(aConnection : TSQLConnection; aTransaction : TSQLTransaction) : TMyTypeSafeAccess;

begin
  Result:=CreateQuery(SQLMyTypeSafeAccess,aConnection,aTransaction);
end;


Class Function TMyTypeSafeAccess.CreateQuery(aSQL : String; aConnection : TSQLConnection; aTransaction : TSQLTransaction) : TMyTypeSafeAccess;

Var
  Q : TSQLQuery;
  MySQL : String;
begin
  If aSQL='' then
    MySQL:=SQLMyTypeSafeAccess 
  else
    MySQL:=aSQL;
  Q:=TSQLQuery.Create(aConnection);
  If aTransaction<>Nil then
    Q.Transaction:=aTransaction;
  Q.Database:=aConnection;
  Q.SQL.Text:=MySQL;
  Result:=TMyTypeSafeAccess.Create(Q,True);
end;


Class Function TMyTypeSafeAccess.GetQuery(aConnection : TSQLConnection; aTransaction : TSQLTransaction) : IMyTypeSafeAccess;

begin
  Result:=CreateQuery(aConnection,aTransaction);
end;


Class Function TMyTypeSafeAccess.GetQuery(aSQL : String; aConnection : TSQLConnection; aTransaction : TSQLTransaction) : IMyTypeSafeAccess;

begin
  Result:=CreateQuery(aSQL,aConnection,aTransaction);
end;


Procedure TMyTypeSafeAccess.ApplyUpdates;


begin
  If Dataset is TSQLQuery then
    (Dataset as TSQLQuery).ApplyUpdates;
end;



end.
