{$INCLUDE sdo_global.inc}
unit test_dataobjectlist;

interface
uses SysUtils
{$IFDEF FPC}
  ,fpcunit, testutils, testregistry
{$ENDIF}
{$IFNDEF FPC}
  ,TestFrameWork
{$ENDIF}
  , test_suite_utils, sdo, sdo_type, sdo_types, sdo_linked_list ;

type

  TSDODataObjectList_BaseTest = class(TWstBaseTest)
  private
    FFactory : ISDODataFactory;
  protected
    class function Create_Factory() : ISDODataFactory;
    class function Create_List(AItemType : ISDOType) : ISDODataObjectList;virtual;abstract;
    function Create_Object() : ISDODataObject;
  protected
    procedure SetUp(); override;
    procedure TearDown(); override;
    procedure CheckEquals(expected, actual: TSDODate; msg: string = ''; const AStrict : Boolean = True); overload;
  published
    procedure append_integer();
    procedure insert_integer();
    procedure get_integer_cursor();
      procedure get_integer_index();
    procedure set_integer_cursor();
      procedure set_integer_index();
    procedure delete_integer_with_cursor();
      procedure delete_integer_with_index();

    procedure append_boolean();
    procedure insert_boolean();
    procedure get_boolean_cursor();
      procedure get_boolean_index();
    procedure set_boolean_cursor();
      procedure set_boolean_index();
    procedure delete_boolean_with_cursor();
      procedure delete_boolean_with_index();

    procedure append_byte();
    procedure insert_byte();
    procedure get_byte_cursor();
      procedure get_byte_index();
    procedure set_byte_cursor();
      procedure set_byte_index();
    procedure delete_byte_with_cursor();
      procedure delete_byte_with_index();

    procedure append_date();
    procedure insert_date();
    procedure get_date_cursor();
      procedure get_date_index();
    procedure set_date_cursor();
      procedure set_date_index();
    procedure delete_date_with_cursor();
      procedure delete_date_with_index();

    procedure append_string();
    procedure insert_string();
    procedure get_string_cursor();
      procedure get_string_index();
    procedure set_string_cursor();
      procedure set_string_index();
    procedure delete_string_with_cursor();
      procedure delete_string_with_index();

    procedure append_dataObject();
    procedure insert_dataObject();
    procedure get_dataObject_cursor();
      procedure get_dataObject_index();
    procedure set_dataObject_cursor();
      procedure set_dataObject_index();
    procedure delete_dataObject_with_cursor();
      procedure delete_dataObject_with_index();

{$IFDEF HAS_SDO_BYTES}
    procedure append_bytes();
    procedure insert_bytes();
    procedure get_bytes_cursor();
      procedure get_bytes_index();
    procedure set_bytes_cursor();
      procedure set_bytes_index();
    procedure delete_bytes_with_cursor();
      procedure delete_bytes_with_index();
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure append_char();
    procedure insert_char();
    procedure get_char_cursor();
      procedure get_char_index();
    procedure set_char_cursor();
      procedure set_char_index();
    procedure delete_char_with_cursor();
      procedure delete_char_with_index();
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure append_currency();
    procedure insert_currency();
    procedure get_currency_cursor();
      procedure get_currency_index();
    procedure set_currency_cursor();
      procedure set_currency_index();
    procedure delete_currency_with_cursor();
      procedure delete_currency_with_index();
{$ENDIF HAS_SDO_CURRENCY}
{$IFDEF HAS_SDO_DOUBLE}
    procedure append_double();
    procedure insert_double();
    procedure get_double_cursor();
      procedure get_double_index();
    procedure set_double_cursor();
      procedure set_double_index();
    procedure delete_double_with_cursor();
      procedure delete_double_with_index();
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure append_float();
    procedure insert_float();
    procedure get_float_cursor();
      procedure get_float_index();
    procedure set_float_cursor();
      procedure set_float_index();
    procedure delete_float_with_cursor();
      procedure delete_float_with_index();
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    procedure append_long();
    procedure insert_long();
    procedure get_long_cursor();
      procedure get_long_index();
    procedure set_long_cursor();
      procedure set_long_index();
    procedure delete_long_with_cursor();
      procedure delete_long_with_index();
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure append_short();
    procedure insert_short();
    procedure get_short_cursor();
      procedure get_short_index();
    procedure set_short_cursor();
      procedure set_short_index();
    procedure delete_short_with_cursor();
      procedure delete_short_with_index();
{$ENDIF HAS_SDO_SHORT}
  end;

  TSDODataObjectList_Test = class(TSDODataObjectList_BaseTest)
  protected
    class function Create_List(AItemType : ISDOType) : ISDODataObjectList;override;
  end;

implementation

uses
  Math, sdo_datafactory, sdo_dataobject,
  DateUtils, sdo_date_utils;

const s_URI_1  = 'uri:1';
      s_TYPE_1 = 'type1';
      s_PROP_BOOL_1 = 'propboolean1';
      s_PROP_BYTE_1 = 'propbyte1';
      s_PROP_INTEGER_1 = 'propinteger1';
      s_PROP_STR_1 = 'propStr1';
        s_PROP_STR_2 = 'propStr2';


{ TSDODataObjectList_Test }

procedure TSDODataObjectList_BaseTest.append_boolean();
var
  vals : array of TSDOBoolean;
  c, i, k : PtrInt;
  ls : ISDODataObjectList;
  crs : ILinkedListCursor;
begin
  c := RandomRange(10,100);
  SetLength(vals,c);
  try
    for i := 0 to Pred(c) do
      vals[i] := ( ( RandomRange(-10000,12345) mod 3 ) = 0 );
    ls := Create_List(TSDOBooleanType.Create(FFactory) as ISDOType);
    crs := ls.getCursor();

    for i := 0 to Pred(c) do begin
      CheckEquals(i, ls.size(), 'size() before');
      ls.append(vals[i]);
      CheckEquals(( i + 1 ),ls.size, 'size() after');
      CheckEquals(True, crs.Eof());
      CheckEquals(vals[i],ls.getBoolean());
      for k := 0 to i do begin
        CheckEquals(vals[k],ls.getBoolean(k), Format('ls.getBoolean(%d)',[k]));
      end;
    end;
  finally
    SetLength(vals,0);
  end;
end;

procedure TSDODataObjectList_BaseTest.append_byte();
var
  vals : array of TSDOByte;
  c, i, k : PtrInt;
  ls : ISDODataObjectList;
  crs : ILinkedListCursor;
begin
  c := RandomRange(10,100);
  SetLength(vals,c);
  try
    for i := 0 to Pred(c) do
      vals[i] := RandomRange(Low(TSDOByte),High(TSDOByte));
    ls := Create_List(TSDOByteType.Create(FFactory) as ISDOType);
    crs := ls.getCursor();

    for i := 0 to Pred(c) do begin
      CheckEquals(i, ls.size(), 'size() before');
      ls.append(vals[i]);
      CheckEquals(( i + 1 ),ls.size, 'size() after');
      CheckEquals(True, crs.Eof());
      CheckEquals(vals[i],ls.getByte());
      for k := 0 to i do begin
        CheckEquals(vals[k],ls.getByte(k), Format('ls.getByte(%d)',[k]));
      end;
    end;
  finally
    SetLength(vals,0);
  end;
end;

procedure TSDODataObjectList_BaseTest.append_dataObject();
var
  vals : array of ISDODataObject;
  c, i, k : PtrInt;
  ls : ISDODataObjectList;
  crs : ILinkedListCursor;
begin
  c := RandomRange(10,100);
  SetLength(vals,c);
  try
    for i := 0 to Pred(c) do
      vals[i] := FFactory.createNew(s_URI_1,s_TYPE_1);
    ls := Create_List(FFactory.getType(s_URI_1,s_TYPE_1));
    crs := ls.getCursor();

    for i := 0 to Pred(c) do begin
      CheckEquals(i, ls.size(), 'size() before');
      ls.append(vals[i]);
      CheckEquals(( i + 1 ),ls.size, 'size() after');
      CheckEquals(True, crs.Eof());
      CheckEquals(PtrUInt(vals[i]),PtrUInt(ls.getDataObject()));
      for k := 0 to i do begin
        CheckEquals(PtrUInt(vals[k]),PtrUInt(ls.getDataObject(k)), Format('ls.getDataObject(%d)',[k]));
      end;
    end;
  finally
    SetLength(vals,0);
  end;
end;

procedure TSDODataObjectList_BaseTest.append_date();
var
  vals : array of TSDODateTime;
  c, i, k : PtrInt;
  ls : ISDODataObjectList;
  crs : ILinkedListCursor;
begin
  c := RandomRange(10,100);
  SetLength(vals,c);
  try
    for i := 0 to Pred(c) do
      vals[i] := RandomDate();
    ls := Create_List(TSDODateTimeType.Create(FFactory) as ISDOType);
    crs := ls.getCursor();

    for i := 0 to Pred(c) do begin
      CheckEquals(i, ls.size(), 'size() before');
      ls.append(vals[i]);
      CheckEquals(( i + 1 ),ls.size, 'size() after');
      CheckEquals(True, crs.Eof());
      CheckEquals(vals[i],ls.getDate());
      for k := 0 to i do begin
        CheckEquals(vals[k],ls.getDate(k), Format('ls.getDate(%d)',[k]));
      end;
    end;
  finally
    SetLength(vals,0);
  end;
end;

procedure TSDODataObjectList_BaseTest.append_integer();
var
  vals : array of TSDOInteger;
  c, i, k : PtrInt;
  ls : ISDODataObjectList;
  crs : ILinkedListCursor;
begin
  c := RandomRange(10,100);
  SetLength(vals,c);
  try
    for i := 0 to Pred(c) do
      vals[i] := RandomRange(-10000,12345);
    ls := Create_List(TSDOIntegerType.Create(FFactory) as ISDOType);
    crs := ls.getCursor();

    for i := 0 to Pred(c) do begin
      CheckEquals(i, ls.size(), 'size() before');
      ls.append(vals[i]);
      CheckEquals(( i + 1 ),ls.size, 'size() after');
      CheckEquals(True, crs.Eof());
      CheckEquals(vals[i],ls.getInteger());
      for k := 0 to i do begin
        CheckEquals(vals[k],ls.getInteger(k), Format('ls.getInteger(%d)',[k]));
      end;
    end;
  finally
    SetLength(vals,0);
  end;
end;

procedure TSDODataObjectList_BaseTest.append_string();
var
  vals : array of TSDOString;
  c, i, k : PtrInt;
  ls : ISDODataObjectList;
  crs : ILinkedListCursor;
begin
  c := RandomRange(10,100);
  SetLength(vals,c);
  try
    for i := 0 to Pred(c) do
      vals[i] := RandomString(RandomRange(0,123));
    ls := Create_List(TSDOStringType.Create(FFactory) as ISDOType);
    crs := ls.getCursor();

    for i := 0 to Pred(c) do begin
      CheckEquals(i, ls.size(), 'size() before');
      ls.append(vals[i]);
      CheckEquals(( i + 1 ),ls.size, 'size() after');
      CheckEquals(True, crs.Eof());
      CheckEquals(vals[i],ls.getString());
      for k := 0 to i do begin
        CheckEquals(vals[k],ls.getString(k), Format('ls.getString(%d)',[k]));
      end;
    end;
  finally
    SetLength(vals,0);
  end;
end;

{$IFDEF HAS_SDO_BYTES}
procedure TSDODataObjectList_BaseTest.append_bytes();
var
  vals : array of TSDOBytes;
  c, i, k : PtrInt;
  ls : ISDODataObjectList;
  crs : ILinkedListCursor;
begin
  c := RandomRange(10,100);
  SetLength(vals,c);
  try
    for i := 0 to Pred(c) do
      vals[i] := RandomBytes(RandomRange(0,100));
    ls := Create_List(TSDOBytesType.Create(FFactory) as ISDOType);
    crs := ls.getCursor();

    for i := 0 to Pred(c) do begin
      CheckEquals(i, ls.size(), 'size() before');
      ls.appendBytes(vals[i]);
      CheckEquals(( i + 1 ),ls.size, 'size() after');
      CheckEquals(True, crs.Eof());
      CheckEquals(vals[i],ls.getBytes());
      for k := 0 to i do begin
        CheckEquals(vals[k],ls.getBytes(k), Format('ls.getBytes(%d)',[k]));
      end;
    end;
  finally
    SetLength(vals,0);
  end;
end;

procedure TSDODataObjectList_BaseTest.insert_bytes();
var
  ls : ISDODataObjectList;

  procedure CheckInvalidIndex(const AIndex : PtrInt; const AValue : TSDOBytes);
  var
    pass : Boolean;
    oldSize : PtrInt;
  begin
    oldSize := ls.size();
    pass := False;
    try
      ls.insertBytes(-10,AValue);
    except
      on e : ESDOIndexOutOfRangeException do begin
        pass := True;
      end;
    end;
    CheckEquals(True,pass, Format('Index = %d',[AIndex]));
    CheckEquals(oldSize, ls.size(), 'A failed "insert" must not modify the list''s size.');
  end;

var
  c : ILinkedListCursor;
  val_1, val_2, val_3 : TSDOBytes;
begin
  val_1 := RandomBytes(RandomRange(0,100));
  val_2 := RandomBytes(RandomRange(0,100));
  val_3 := RandomBytes(RandomRange(0,100));
  ls := Create_List(TSDOBytesType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  CheckInvalidIndex(-12, val_1);
  CheckInvalidIndex(-1, val_1);
  CheckInvalidIndex(1, val_1);
  CheckInvalidIndex(12, val_1);

  ls.insertBytes(0,val_1);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_1, ls.getBytes(0));
    ls.insertBytes(1,val_2);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getBytes(0));
      CheckEquals(val_2, ls.getBytes(1));
      ls.insertBytes(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getBytes(0));
        CheckEquals(val_2, ls.getBytes(1));
        CheckEquals(val_3, ls.getBytes(2));

  ls := Create_List(TSDOBytesType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insertBytes(0,val_2);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_2, ls.getBytes(0));
    ls.insertBytes(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getBytes(0));
      CheckEquals(val_2, ls.getBytes(1));
      ls.insertBytes(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getBytes(0));
        CheckEquals(val_2, ls.getBytes(1));
        CheckEquals(val_3, ls.getBytes(2));

  ls := Create_List(TSDOBytesType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insertBytes(0,val_3);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_3, ls.getBytes(0));
    ls.insertBytes(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getBytes(0));
      CheckEquals(val_3, ls.getBytes(1));
      ls.insertBytes(1,val_2);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getBytes(0));
        CheckEquals(val_2, ls.getBytes(1));
        CheckEquals(val_3, ls.getBytes(2));
end;

procedure TSDODataObjectList_BaseTest.get_bytes_cursor();
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
  VAL_1, VAL_2, VAL_3 : TSDOBytes;
begin
  VAL_1 := RandomBytes(RandomRange(0,100));
  VAL_2 := RandomBytes(RandomRange(0,100));
  VAL_3 := nil;
  ls := Create_List(TSDOBytesType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getBytes();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.appendBytes(TSDOBytes(nil));
  Check(c.MoveLast());
  PPSDOBytes(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_1;
  CheckEquals(1,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getBytes());
    CheckEquals(False,c.MoveNext());
    PPSDOBytes(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_2;
    CheckEquals(VAL_2,ls.getBytes());
    PPSDOBytes(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_1;

  ls.appendBytes(TSDOBytes(nil));
  Check(c.MoveLast());
  PPSDOBytes(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_2;
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getBytes());
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,ls.getBytes());
    CheckEquals(False,c.MoveNext());

  ls.appendBytes(TSDOBytes(nil));
  Check(c.MoveLast());
  PPSDOBytes(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_3;
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,ls.getBytes());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,ls.getBytes());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,ls.getBytes());
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOBytesType.Create(FFactory));
  c := ls.getCursor();
  ls.appendBytes(TSDOBytes(nil));
  ls.appendBytes(TSDOBytes(nil));
  ls.appendBytes(TSDOBytes(nil));
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(TSDOBytes(nil),ls.getBytes());
    CheckEquals(True,c.MoveNext());
      CheckEquals(TSDOBytes(nil),ls.getBytes());
    CheckEquals(True,c.MoveNext());
      CheckEquals(TSDOBytes(nil),ls.getBytes());
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.get_bytes_index();
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
  VAL_1, VAL_2, VAL_3 : TSDOBytes;
begin
  VAL_1 := RandomBytes(RandomRange(0,100));
  VAL_2 := RandomBytes(RandomRange(0,100));
  VAL_3 := nil;
  ls := Create_List(TSDOBytesType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getBytes(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.appendBytes(TSDOBytes(nil));
  Check(c.MoveLast());
  PPSDOBytes(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_1;
  CheckEquals(1,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,ls.getBytes(0));
    PPSDOBytes(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_2;
    CheckEquals(VAL_2,ls.getBytes(0));
    PPSDOBytes(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_1;

  ls.appendBytes(TSDOBytes(nil));
  Check(c.MoveLast());
  PPSDOBytes(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_2;
    CheckEquals(2,ls.size());
    CheckEquals(VAL_1,ls.getBytes(0));
    CheckEquals(VAL_2,ls.getBytes(1));

  ls.appendBytes(TSDOBytes(nil));
  Check(c.MoveLast());
  PPSDOBytes(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_3;
      CheckEquals(3,ls.size());
      CheckEquals(VAL_1,ls.getBytes(0));
      CheckEquals(VAL_2,ls.getBytes(1));
      CheckEquals(VAL_3,ls.getBytes(2));

  ls := Create_List(TSDOBytesType.Create(FFactory));
  c := ls.getCursor();
  ls.appendBytes(TSDOBytes(nil));
  ls.appendBytes(TSDOBytes(nil));
  ls.appendBytes(TSDOBytes(nil));
    CheckEquals(3,ls.size());
    CheckEquals(TSDOBytes(nil),ls.getBytes(0));
    CheckEquals(TSDOBytes(nil),ls.getBytes(1));
    CheckEquals(TSDOBytes(nil),ls.getBytes(2));

  ok := False;
  try
    ls.getBytes(3);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);
end;

procedure TSDODataObjectList_BaseTest.set_bytes_cursor();
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
  VAL_1, VAL_2, VAL_3 : TSDOBytes;
begin
  VAL_1 := RandomBytes(RandomRange(0,100));
  VAL_2 := RandomBytes(RandomRange(0,100));
  VAL_3 := nil;
  ls := Create_List(TSDOBytesType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setBytes(VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.appendBytes(TSDOBytes(nil));
  c.MoveFirst();
  ls.setBytes(VAL_1);
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PPSDOBytes( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());
    ls.setBytes(VAL_2);
    CheckEquals(VAL_2,PPSDOBytes( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    ls.setBytes(VAL_1);

  ls.appendBytes(TSDOBytes(nil));
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  ls.setBytes(VAL_2);
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PPSDOBytes( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PPSDOBytes( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());

  ls.appendBytes(TSDOBytes(nil));
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  CheckEquals(True,c.MoveNext());
  ls.setBytes(VAL_3);
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,PPSDOBytes( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PPSDOBytes( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PPSDOBytes( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOBytesType.Create(FFactory));
  c := ls.getCursor();
  ls.appendBytes(VAL_1);
  ls.appendBytes(VAL_2);
  ls.appendBytes(VAL_3);
  CheckEquals(True,c.MoveFirst());
    ls.setBytes(nil);
  CheckEquals(True,c.MoveNext());
    ls.setBytes(nil);
  CheckEquals(True,c.MoveNext());
    ls.setBytes(nil);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(TSDOBytes(nil),PPSDOBytes( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(TSDOBytes(nil),PPSDOBytes( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(TSDOBytes(nil),PPSDOBytes( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.set_bytes_index();
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
  VAL_1, VAL_2, VAL_3 : TSDOBytes;
begin
  VAL_1 := RandomBytes(RandomRange(0,100));
  VAL_2 := RandomBytes(RandomRange(0,100));
  VAL_3 := nil;
  ls := Create_List(TSDOBytesType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setBytes(1,VAL_1);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.appendBytes(TSDOBytes(nil));
  ls.setBytes(0,VAL_1);
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PPSDOBytes( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());
    ls.setBytes(0,VAL_2);
    CheckEquals(VAL_2,PPSDOBytes( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    ls.setBytes(0,VAL_1);

  ls.appendBytes(TSDOBytes(nil));
  ls.setBytes(1,VAL_2);
  CheckEquals(2,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PPSDOBytes( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PPSDOBytes( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());

  ls.appendBytes(TSDOBytes(nil));
  ls.setBytes(2,VAL_3);
  CheckEquals(3,ls.size());
    CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,PPSDOBytes( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PPSDOBytes( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PPSDOBytes( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOBytesType.Create(FFactory));
  c := ls.getCursor();
  ls.appendBytes(VAL_1);
  ls.appendBytes(VAL_2);
  ls.appendBytes(VAL_3);
  ls.setBytes(0,nil);
  ls.setBytes(1,nil);
  ls.setBytes(2,nil);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(TSDOBytes(nil),PPSDOBytes( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(TSDOBytes(nil),PPSDOBytes( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(TSDOBytes(nil),PPSDOBytes( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.delete_bytes_with_cursor();
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
  VAL_1, VAL_2, VAL_3 : TSDOBytes;
begin
  VAL_1 := RandomBytes(RandomRange(0,100));
  VAL_2 := RandomBytes(RandomRange(0,100));
  VAL_3 := nil;
  ls := Create_List(TSDOBytesType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.appendBytes(VAL_1);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
    CheckEquals(0,ls.size());

  ls.appendBytes(VAL_1);
  ls.appendBytes(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getBytes());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.appendBytes(VAL_1);
  ls.appendBytes(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,ls.getBytes());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.appendBytes(VAL_1);
  ls.appendBytes(VAL_2);
  ls.appendBytes(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getBytes());
      CheckEquals(2,ls.size());
      CheckEquals(True,c.MoveNext());
      ls.delete();
        CheckEquals(True,c.MoveFirst());
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getBytes());
        ls.delete();
        CheckEquals(0,ls.size());

  ls.appendBytes(VAL_1);
  ls.appendBytes(VAL_2);
  ls.appendBytes(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveLast());
      CheckEquals(VAL_2,ls.getBytes());
      CheckEquals(2,ls.size());
      ls.delete();
        CheckEquals(1,ls.size());
        CheckEquals(True,c.MoveLast());
        CheckEquals(VAL_1,ls.getBytes());
        ls.delete();
        CheckEquals(0,ls.size());
end;

procedure TSDODataObjectList_BaseTest.delete_bytes_with_index();
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
  VAL_1, VAL_2, VAL_3 : TSDOBytes;
begin
  VAL_1 := RandomBytes(RandomRange(0,100));
  VAL_2 := RandomBytes(RandomRange(0,100));
  VAL_3 := nil;
  ls := Create_List(TSDOBytesType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ok := False;
  try
    ls.delete(123);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.appendBytes(VAL_1);
    ls.delete(0);
    CheckEquals(0,ls.size());

  ls.appendBytes(VAL_1);
  ls.appendBytes(VAL_2);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getBytes(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.appendBytes(VAL_1);
  ls.appendBytes(VAL_2);
    ls.delete(1);
      CheckEquals(VAL_1,ls.getBytes(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.appendBytes(VAL_1);
  ls.appendBytes(VAL_2);
  ls.appendBytes(VAL_3);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getBytes(0));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getBytes(0));
        ls.delete(0);
        CheckEquals(0,ls.size());

  ls.appendBytes(VAL_1);
  ls.appendBytes(VAL_2);
  ls.appendBytes(VAL_3);
    c.Reset();
    ls.delete(2);
      CheckEquals(VAL_2,ls.getBytes(1));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_1,ls.getBytes(0));
        ls.delete(0);
        CheckEquals(0,ls.size());
end;
{$ENDIF HAS_SDO_BYTES}

{$IFDEF HAS_SDO_CHAR}
procedure TSDODataObjectList_BaseTest.append_char();
var
  vals : array of TSDOChar;
  c, i, k : PtrInt;
  ls : ISDODataObjectList;
  crs : ILinkedListCursor;
begin
  c := RandomRange(10,100);
  SetLength(vals,c);
  try
    for i := 0 to Pred(c) do
      vals[i] := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));
    ls := Create_List(TSDOCharacterType.Create(FFactory) as ISDOType);
    crs := ls.getCursor();

    for i := 0 to Pred(c) do begin
      CheckEquals(i, ls.size(), 'size() before');
      ls.append(vals[i]);
      CheckEquals(( i + 1 ),ls.size, 'size() after');
      CheckEquals(True, crs.Eof());
      CheckEquals(vals[i],ls.getCharacter());
      for k := 0 to i do begin
        CheckEquals(vals[k],ls.getCharacter(k), Format('ls.getCharacter(%d)',[k]));
      end;
    end;
  finally
    SetLength(vals,0);
  end;
end;

procedure TSDODataObjectList_BaseTest.insert_char();
var
  ls : ISDODataObjectList;

  procedure CheckInvalidIndex(const AIndex : PtrInt; const AValue : TSDOChar);
  var
    pass : Boolean;
    oldSize : PtrInt;
  begin
    oldSize := ls.size();
    pass := False;
    try
      ls.insert(-10,AValue);
    except
      on e : ESDOIndexOutOfRangeException do begin
        pass := True;
      end;
    end;
    CheckEquals(True,pass, Format('Index = %d',[AIndex]));
    CheckEquals(oldSize, ls.size(), 'A failed "insert" must not modify the list''s size.');
  end;

var
  c : ILinkedListCursor;
  val_1, val_2, val_3 : TSDOChar;
begin
  val_1 := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));
  val_2 := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));
  val_3 := TSDOChar(RandomRange(Ord(Low(TSDOChar)),Ord(High(TSDOChar))));
  ls := Create_List(TSDOCharacterType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  CheckInvalidIndex(-12, val_1);
  CheckInvalidIndex(-1, val_1);
  CheckInvalidIndex(1, val_1);
  CheckInvalidIndex(12, val_1);

  ls.insert(0,val_1);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_1, ls.getCharacter(0));
    ls.insert(1,val_2);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getCharacter(0));
      CheckEquals(val_2, ls.getCharacter(1));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getCharacter(0));
        CheckEquals(val_2, ls.getCharacter(1));
        CheckEquals(val_3, ls.getCharacter(2));

  ls := Create_List(TSDOCharacterType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insert(0,val_2);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_2, ls.getCharacter(0));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getCharacter(0));
      CheckEquals(val_2, ls.getCharacter(1));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getCharacter(0));
        CheckEquals(val_2, ls.getCharacter(1));
        CheckEquals(val_3, ls.getCharacter(2));

  ls := Create_List(TSDOCharacterType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insert(0,val_3);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_3, ls.getCharacter(0));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getCharacter(0));
      CheckEquals(val_3, ls.getCharacter(1));
      ls.insert(1,val_2);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getCharacter(0));
        CheckEquals(val_2, ls.getCharacter(1));
        CheckEquals(val_3, ls.getCharacter(2));
end;

procedure TSDODataObjectList_BaseTest.get_char_cursor();
const VAL_1 : TSDOChar = 'y'; VAL_2 : TSDOChar = '8'; VAL_3 : TSDOChar = #0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOCharacterType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getCharacter();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(TSDOChar(0));
  Check(c.MoveLast());
  PSDOChar(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;
  CheckEquals(1,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getCharacter());
    CheckEquals(False,c.MoveNext());
    PSDOChar(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(VAL_2,ls.getCharacter());
    PSDOChar(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;

  ls.append(TSDOChar(0));
  Check(c.MoveLast());
  PSDOChar(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getCharacter());
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,ls.getCharacter());
    CheckEquals(False,c.MoveNext());

  ls.append(TSDOChar(0));
  Check(c.MoveLast());
  PSDOChar(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_3;
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,ls.getCharacter());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,ls.getCharacter());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,ls.getCharacter());
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOCharacterType.Create(FFactory));
  c := ls.getCursor();
  ls.append(TSDOChar(0));
  ls.append(TSDOChar(0));
  ls.append(TSDOChar(0));
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(TSDOChar(0),ls.getCharacter());
    CheckEquals(True,c.MoveNext());
      CheckEquals(TSDOChar(0),ls.getCharacter());
    CheckEquals(True,c.MoveNext());
      CheckEquals(TSDOChar(0),ls.getCharacter());
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.get_char_index();
const VAL_1 : TSDOChar = 'y'; VAL_2 : TSDOChar = '8'; VAL_3 : TSDOChar = #0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOCharacterType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getCharacter(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(TSDOChar(0));
  Check(c.MoveLast());
  PSDOChar(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;
  CheckEquals(1,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,ls.getCharacter(0));
    PSDOChar(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(VAL_2,ls.getCharacter(0));
    PSDOChar(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;

  ls.append(TSDOChar(0));
  Check(c.MoveLast());
  PSDOChar(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(2,ls.size());
    CheckEquals(VAL_1,ls.getCharacter(0));
    CheckEquals(VAL_2,ls.getCharacter(1));

  ls.append(TSDOChar(0));
  Check(c.MoveLast());
  PSDOChar(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_3;
      CheckEquals(3,ls.size());
      CheckEquals(VAL_1,ls.getCharacter(0));
      CheckEquals(VAL_2,ls.getCharacter(1));
      CheckEquals(VAL_3,ls.getCharacter(2));

  ls := Create_List(TSDOCharacterType.Create(FFactory));
  c := ls.getCursor();
  ls.append(TSDOChar(0));
  ls.append(TSDOChar(0));
  ls.append(TSDOChar(0));
    CheckEquals(3,ls.size());
    CheckEquals(TSDOChar(0),ls.getCharacter(0));
    CheckEquals(TSDOChar(0),ls.getCharacter(1));
    CheckEquals(TSDOChar(0),ls.getCharacter(2));

  ok := False;
  try
    ls.getCharacter(3);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);
end;

procedure TSDODataObjectList_BaseTest.set_char_cursor();
const VAL_1 : TSDOChar = 'y'; VAL_2 : TSDOChar = '8'; VAL_3 : TSDOChar = #0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOCharacterType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setCharacter(VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(TSDOChar(0));
  c.MoveFirst();
  ls.setCharacter(VAL_1);
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PSDOChar( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
    ls.setCharacter(VAL_2);
    CheckEquals(VAL_2,PSDOChar( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    ls.setCharacter(VAL_1);

  ls.append(TSDOChar(0));
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  ls.setCharacter(VAL_2);
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PSDOChar( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PSDOChar( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls.append(TSDOChar(0));
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  CheckEquals(True,c.MoveNext());
  ls.setCharacter(VAL_3);
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,PSDOChar( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PSDOChar( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PSDOChar( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOCharacterType.Create(FFactory));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  CheckEquals(True,c.MoveFirst());
    ls.setCharacter(TSDOChar(0));
  CheckEquals(True,c.MoveNext());
    ls.setCharacter(TSDOChar(0));
  CheckEquals(True,c.MoveNext());
    ls.setCharacter(TSDOChar(0));

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(TSDOChar(0),PSDOChar( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(TSDOChar(0),PSDOChar( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(TSDOChar(0),PSDOChar( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.set_char_index();
const VAL_1 : TSDOChar = 'y'; VAL_2 : TSDOChar = '8'; VAL_3 : TSDOChar = #0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOCharacterType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setCharacter(1,VAL_1);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(TSDOChar(0));
  ls.setCharacter(0,VAL_1);
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PSDOChar( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
    ls.setCharacter(0,VAL_2);
    CheckEquals(VAL_2,PSDOChar( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    ls.setCharacter(0,VAL_1);

  ls.append(TSDOChar(0));
  ls.setCharacter(1,VAL_2);
  CheckEquals(2,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PSDOChar( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PSDOChar( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls.append(TSDOChar(0));
  ls.setCharacter(2,VAL_3);
  CheckEquals(3,ls.size());
    CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,PSDOChar( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PSDOChar( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PSDOChar( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOCharacterType.Create(FFactory));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  ls.setCharacter(0,TSDOChar(0));
  ls.setCharacter(1,TSDOChar(0));
  ls.setCharacter(2,TSDOChar(0));

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(TSDOChar(0),PSDOChar( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(TSDOChar(0),PSDOChar( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(TSDOChar(0),PSDOChar( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.delete_char_with_cursor();
const VAL_1 : TSDOChar = 'y'; VAL_2 : TSDOChar = '8'; VAL_3 : TSDOChar = #0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOCharacterType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getCharacter());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,ls.getCharacter());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getCharacter());
      CheckEquals(2,ls.size());
      CheckEquals(True,c.MoveNext());
      ls.delete();
        CheckEquals(True,c.MoveFirst());
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getCharacter());
        ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveLast());
      CheckEquals(VAL_2,ls.getCharacter());
      CheckEquals(2,ls.size());
      ls.delete();
        CheckEquals(1,ls.size());
        CheckEquals(True,c.MoveLast());
        CheckEquals(VAL_1,ls.getCharacter());
        ls.delete();
        CheckEquals(0,ls.size());
end;

procedure TSDODataObjectList_BaseTest.delete_char_with_index();
const VAL_1 : TSDOChar = 'y'; VAL_2 : TSDOChar = '8'; VAL_3 : TSDOChar = #0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOCharacterType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ok := False;
  try
    ls.delete(123);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    ls.delete(0);
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getCharacter(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(1);
      CheckEquals(VAL_1,ls.getCharacter(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getCharacter(0));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getCharacter(0));
        ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    ls.delete(2);
      CheckEquals(VAL_2,ls.getCharacter(1));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_1,ls.getCharacter(0));
        ls.delete(0);
        CheckEquals(0,ls.size());
end;
{$ENDIF HAS_SDO_CHAR}

{$IFDEF HAS_SDO_CURRENCY}
procedure TSDODataObjectList_BaseTest.append_currency();
var
  vals : array of TSDOCurrency;
  c, i, k : PtrInt;
  ls : ISDODataObjectList;
  crs : ILinkedListCursor;
begin
  c := RandomRange(10,100);
  SetLength(vals,c);
  try
    for i := 0 to Pred(c) do
      vals[i] := RandomRange(Low(TSDOInteger),High(TSDOInteger));
    ls := Create_List(TSDOCurrencyType.Create(FFactory) as ISDOType);
    crs := ls.getCursor();

    for i := 0 to Pred(c) do begin
      CheckEquals(i, ls.size(), 'size() before');
      ls.appendCurrency(vals[i]);
      CheckEquals(( i + 1 ),ls.size, 'size() after');
      CheckEquals(True, crs.Eof());
      CheckEquals(vals[i],ls.getCurrency());
      for k := 0 to i do begin
        CheckEquals(vals[k],ls.getCurrency(k), Format('ls.getCurrency(%d)',[k]));
      end;
    end;
  finally
    SetLength(vals,0);
  end;
end;

procedure TSDODataObjectList_BaseTest.insert_currency();
var
  ls : ISDODataObjectList;

  procedure CheckInvalidIndex(const AIndex : PtrInt; const AValue : TSDOCurrency);
  var
    pass : Boolean;
    oldSize : PtrInt;
  begin
    oldSize := ls.size();
    pass := False;
    try
      ls.insertCurrency(-10,AValue);
    except
      on e : ESDOIndexOutOfRangeException do begin
        pass := True;
      end;
    end;
    CheckEquals(True,pass, Format('Index = %d',[AIndex]));
    CheckEquals(oldSize, ls.size(), 'A failed "insertCurrency" must not modify the list''s size.');
  end;

var
  c : ILinkedListCursor;
  val_1, val_2, val_3 : TSDOCurrency;
begin
  val_1 := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  val_2 := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  val_3 := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  ls := Create_List(TSDOCurrencyType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  CheckInvalidIndex(-12, val_1);
  CheckInvalidIndex(-1, val_1);
  CheckInvalidIndex(1, val_1);
  CheckInvalidIndex(12, val_1);

  ls.insertCurrency(0,val_1);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_1, ls.getCurrency(0));
    ls.insertCurrency(1,val_2);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getCurrency(0));
      CheckEquals(val_2, ls.getCurrency(1));
      ls.insertCurrency(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getCurrency(0));
        CheckEquals(val_2, ls.getCurrency(1));
        CheckEquals(val_3, ls.getCurrency(2));

  ls := Create_List(TSDOCurrencyType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insertCurrency(0,val_2);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_2, ls.getCurrency(0));
    ls.insertCurrency(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getCurrency(0));
      CheckEquals(val_2, ls.getCurrency(1));
      ls.insertCurrency(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getCurrency(0));
        CheckEquals(val_2, ls.getCurrency(1));
        CheckEquals(val_3, ls.getCurrency(2));

  ls := Create_List(TSDOCurrencyType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insertCurrency(0,val_3);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_3, ls.getCurrency(0));
    ls.insertCurrency(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getCurrency(0));
      CheckEquals(val_3, ls.getCurrency(1));
      ls.insertCurrency(1,val_2);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getCurrency(0));
        CheckEquals(val_2, ls.getCurrency(1));
        CheckEquals(val_3, ls.getCurrency(2));
end;

procedure TSDODataObjectList_BaseTest.get_currency_cursor();
const VAL_1 : TSDOCurrency = 8877552292000; VAL_2 : TSDOCurrency = -13654792522; VAL_3 : TSDOCurrency = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOCurrencyType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getCurrency();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.appendCurrency(0);
  Check(c.MoveLast());
  PSDOCurrency(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;
  CheckEquals(1,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getCurrency());
    CheckEquals(False,c.MoveNext());
    PSDOCurrency(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(VAL_2,ls.getCurrency());
    PSDOCurrency(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;

  ls.appendCurrency(0);
  Check(c.MoveLast());
  PSDOCurrency(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getCurrency());
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,ls.getCurrency());
    CheckEquals(False,c.MoveNext());

  ls.appendCurrency(0);
  Check(c.MoveLast());
  PSDOCurrency(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_3;
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,ls.getCurrency());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,ls.getCurrency());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,ls.getCurrency());
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOCurrencyType.Create(FFactory));
  c := ls.getCursor();
  ls.appendCurrency(0);
  ls.appendCurrency(0);
  ls.appendCurrency(0);
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,ls.getCurrency());
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,ls.getCurrency());
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,ls.getCurrency());
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.get_currency_index();
const VAL_1 : TSDOCurrency = 8877552292000; VAL_2 : TSDOCurrency = -13654792522; VAL_3 : TSDOCurrency = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOCurrencyType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getCurrency(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.appendCurrency(0);
  Check(c.MoveLast());
  PSDOCurrency(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;
  CheckEquals(1,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,ls.getCurrency(0));
    PSDOCurrency(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(VAL_2,ls.getCurrency(0));
    PSDOCurrency(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;

  ls.appendCurrency(0);
  Check(c.MoveLast());
  PSDOCurrency(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(2,ls.size());
    CheckEquals(VAL_1,ls.getCurrency(0));
    CheckEquals(VAL_2,ls.getCurrency(1));

  ls.appendCurrency(0);
  Check(c.MoveLast());
  PSDOCurrency(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_3;
      CheckEquals(3,ls.size());
      CheckEquals(VAL_1,ls.getCurrency(0));
      CheckEquals(VAL_2,ls.getCurrency(1));
      CheckEquals(VAL_3,ls.getCurrency(2));

  ls := Create_List(TSDOCurrencyType.Create(FFactory));
  c := ls.getCursor();
  ls.appendCurrency(0);
  ls.appendCurrency(0);
  ls.appendCurrency(0);
    CheckEquals(3,ls.size());
    CheckEquals(0,ls.getCurrency(0));
    CheckEquals(0,ls.getCurrency(1));
    CheckEquals(0,ls.getCurrency(2));

  ok := False;
  try
    ls.getCurrency(3);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);
end;

procedure TSDODataObjectList_BaseTest.set_currency_cursor();
const VAL_1 : TSDOCurrency = 8877552292000; VAL_2 : TSDOCurrency = -13654792522; VAL_3 : TSDOCurrency = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOCurrencyType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setCurrency(VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.appendCurrency(0);
  c.MoveFirst();
  ls.setCurrency(VAL_1);
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PSDOCurrency( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
    ls.setCurrency(VAL_2);
    CheckEquals(VAL_2,PSDOCurrency( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    ls.setCurrency(VAL_1);

  ls.appendCurrency(0);
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  ls.setCurrency(VAL_2);
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PSDOCurrency( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PSDOCurrency( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls.appendCurrency(0);
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  CheckEquals(True,c.MoveNext());
  ls.setCurrency(VAL_3);
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,PSDOCurrency( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PSDOCurrency( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PSDOCurrency( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOCurrencyType.Create(FFactory));
  c := ls.getCursor();
  ls.appendCurrency(VAL_1);
  ls.appendCurrency(VAL_2);
  ls.appendCurrency(VAL_3);
  CheckEquals(True,c.MoveFirst());
    ls.setCurrency(0);
  CheckEquals(True,c.MoveNext());
    ls.setCurrency(0);
  CheckEquals(True,c.MoveNext());
    ls.setCurrency(0);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOCurrency( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOCurrency( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOCurrency( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.set_currency_index();
const VAL_1 : TSDOCurrency = 8877552292000; VAL_2 : TSDOCurrency = -13654792522; VAL_3 : TSDOCurrency = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOCurrencyType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setCurrency(1,VAL_1);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.appendCurrency(0);
  ls.setCurrency(0,VAL_1);
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PSDOCurrency( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
    ls.setCurrency(0,VAL_2);
    CheckEquals(VAL_2,PSDOCurrency( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    ls.setCurrency(0,VAL_1);

  ls.appendCurrency(0);
  ls.setCurrency(1,VAL_2);
  CheckEquals(2,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PSDOCurrency( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PSDOCurrency( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls.appendCurrency(0);
  ls.setCurrency(2,VAL_3);
  CheckEquals(3,ls.size());
    CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,PSDOCurrency( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PSDOCurrency( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PSDOCurrency( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOCurrencyType.Create(FFactory));
  c := ls.getCursor();
  ls.appendCurrency(VAL_1);
  ls.appendCurrency(VAL_2);
  ls.appendCurrency(VAL_3);
  ls.setCurrency(0,0);
  ls.setCurrency(1,0);
  ls.setCurrency(2,0);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOCurrency( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOCurrency( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOCurrency( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.delete_currency_with_cursor();
const VAL_1 : TSDOCurrency = 8877552292000; VAL_2 : TSDOCurrency = -13654792522; VAL_3 : TSDOCurrency = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOCurrencyType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.appendCurrency(VAL_1);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
    CheckEquals(0,ls.size());

  ls.appendCurrency(VAL_1);
  ls.appendCurrency(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getCurrency());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.appendCurrency(VAL_1);
  ls.appendCurrency(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,ls.getCurrency());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.appendCurrency(VAL_1);
  ls.appendCurrency(VAL_2);
  ls.appendCurrency(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getCurrency());
      CheckEquals(2,ls.size());
      CheckEquals(True,c.MoveNext());
      ls.delete();
        CheckEquals(True,c.MoveFirst());
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getCurrency());
        ls.delete();
        CheckEquals(0,ls.size());

  ls.appendCurrency(VAL_1);
  ls.appendCurrency(VAL_2);
  ls.appendCurrency(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveLast());
      CheckEquals(VAL_2,ls.getCurrency());
      CheckEquals(2,ls.size());
      ls.delete();
        CheckEquals(1,ls.size());
        CheckEquals(True,c.MoveLast());
        CheckEquals(VAL_1,ls.getCurrency());
        ls.delete();
        CheckEquals(0,ls.size());
end;

procedure TSDODataObjectList_BaseTest.delete_currency_with_index();
const VAL_1 : TSDOCurrency = 8877552292000; VAL_2 : TSDOCurrency = -13654792522; VAL_3 : TSDOCurrency = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOCurrencyType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ok := False;
  try
    ls.delete(123);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.appendCurrency(VAL_1);
    ls.delete(0);
    CheckEquals(0,ls.size());

  ls.appendCurrency(VAL_1);
  ls.appendCurrency(VAL_2);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getCurrency(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.appendCurrency(VAL_1);
  ls.appendCurrency(VAL_2);
    ls.delete(1);
      CheckEquals(VAL_1,ls.getCurrency(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.appendCurrency(VAL_1);
  ls.appendCurrency(VAL_2);
  ls.appendCurrency(VAL_3);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getCurrency(0));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getCurrency(0));
        ls.delete(0);
        CheckEquals(0,ls.size());

  ls.appendCurrency(VAL_1);
  ls.appendCurrency(VAL_2);
  ls.appendCurrency(VAL_3);
    c.Reset();
    ls.delete(2);
      CheckEquals(VAL_2,ls.getCurrency(1));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_1,ls.getCurrency(0));
        ls.delete(0);
        CheckEquals(0,ls.size());
end;
{$ENDIF HAS_SDO_CURRENCY}

{$IFDEF HAS_SDO_DOUBLE}
const ZERO_DOUBLE : TSDODouble = 0;
procedure TSDODataObjectList_BaseTest.append_double();
var
  vals : array of TSDODouble;
  c, i, k : PtrInt;
  ls : ISDODataObjectList;
  crs : ILinkedListCursor;
begin
  c := RandomRange(10,100);
  SetLength(vals,c);
  try
    for i := 0 to Pred(c) do
      vals[i] := RandomRange(Low(TSDOInteger),High(TSDOInteger));
    ls := Create_List(TSDODoubleType.Create(FFactory) as ISDOType);
    crs := ls.getCursor();

    for i := 0 to Pred(c) do begin
      CheckEquals(i, ls.size(), 'size() before');
      ls.append(vals[i]);
      CheckEquals(( i + 1 ),ls.size, 'size() after');
      CheckEquals(True, crs.Eof());
      CheckEquals(vals[i],ls.getDouble());
      for k := 0 to i do begin
        CheckEquals(vals[k],ls.getDouble(k), Format('ls.getDouble(%d)',[k]));
      end;
    end;
  finally
    SetLength(vals,0);
  end;
end;

procedure TSDODataObjectList_BaseTest.insert_double();
var
  ls : ISDODataObjectList;

  procedure CheckInvalidIndex(const AIndex : PtrInt; const AValue : TSDODouble);
  var
    pass : Boolean;
    oldSize : PtrInt;
  begin
    oldSize := ls.size();
    pass := False;
    try
      ls.insert(-10,AValue);
    except
      on e : ESDOIndexOutOfRangeException do begin
        pass := True;
      end;
    end;
    CheckEquals(True,pass, Format('Index = %d',[AIndex]));
    CheckEquals(oldSize, ls.size(), 'A failed "insert" must not modify the list''s size.');
  end;

var
  c : ILinkedListCursor;
  val_1, val_2, val_3 : TSDODouble;
begin
  val_1 := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  val_2 := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  val_3 := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  ls := Create_List(TSDODoubleType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  CheckInvalidIndex(-12, val_1);
  CheckInvalidIndex(-1, val_1);
  CheckInvalidIndex(1, val_1);
  CheckInvalidIndex(12, val_1);

  ls.insert(0,val_1);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_1, ls.getDouble(0));
    ls.insert(1,val_2);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getDouble(0));
      CheckEquals(val_2, ls.getDouble(1));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getDouble(0));
        CheckEquals(val_2, ls.getDouble(1));
        CheckEquals(val_3, ls.getDouble(2));

  ls := Create_List(TSDODoubleType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insert(0,val_2);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_2, ls.getDouble(0));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getDouble(0));
      CheckEquals(val_2, ls.getDouble(1));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getDouble(0));
        CheckEquals(val_2, ls.getDouble(1));
        CheckEquals(val_3, ls.getDouble(2));

  ls := Create_List(TSDODoubleType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insert(0,val_3);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_3, ls.getDouble(0));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getDouble(0));
      CheckEquals(val_3, ls.getDouble(1));
      ls.insert(1,val_2);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getDouble(0));
        CheckEquals(val_2, ls.getDouble(1));
        CheckEquals(val_3, ls.getDouble(2));
end;

procedure TSDODataObjectList_BaseTest.get_double_cursor();
const VAL_1 : TSDODouble = 8877552292000; VAL_2 : TSDODouble = -13654792522; VAL_3 : TSDODouble = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDODoubleType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getDouble();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(ZERO_DOUBLE);
  Check(c.MoveLast());
  PSDODouble(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;
  CheckEquals(1,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getDouble());
    CheckEquals(False,c.MoveNext());
    PSDODouble(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(VAL_2,ls.getDouble());
    PSDODouble(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;

  ls.append(ZERO_DOUBLE);
  Check(c.MoveLast());
  PSDODouble(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getDouble());
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,ls.getDouble());
    CheckEquals(False,c.MoveNext());

  ls.append(ZERO_DOUBLE);
  Check(c.MoveLast());
  PSDODouble(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_3;
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,ls.getDouble());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,ls.getDouble());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,ls.getDouble());
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDODoubleType.Create(FFactory));
  c := ls.getCursor();
  ls.append(ZERO_DOUBLE);
  ls.append(ZERO_DOUBLE);
  ls.append(ZERO_DOUBLE);
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,ls.getDouble());
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,ls.getDouble());
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,ls.getDouble());
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.get_double_index();
const VAL_1 : TSDODouble = 8877552292000; VAL_2 : TSDODouble = -13654792522; VAL_3 : TSDODouble = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDODoubleType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getDouble(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(ZERO_DOUBLE);
  Check(c.MoveLast());
  PSDODouble(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;
  CheckEquals(1,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,ls.getDouble(0));
    PSDODouble(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(VAL_2,ls.getDouble(0));
    PSDODouble(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;

  ls.append(ZERO_DOUBLE);
  Check(c.MoveLast());
  PSDODouble(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(2,ls.size());
    CheckEquals(VAL_1,ls.getDouble(0));
    CheckEquals(VAL_2,ls.getDouble(1));

  ls.append(ZERO_DOUBLE);
  Check(c.MoveLast());
  PSDODouble(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_3;
      CheckEquals(3,ls.size());
      CheckEquals(VAL_1,ls.getDouble(0));
      CheckEquals(VAL_2,ls.getDouble(1));
      CheckEquals(VAL_3,ls.getDouble(2));

  ls := Create_List(TSDODoubleType.Create(FFactory));
  c := ls.getCursor();
  ls.append(ZERO_DOUBLE);
  ls.append(ZERO_DOUBLE);
  ls.append(ZERO_DOUBLE);
    CheckEquals(3,ls.size());
    CheckEquals(0,ls.getDouble(0));
    CheckEquals(0,ls.getDouble(1));
    CheckEquals(0,ls.getDouble(2));

  ok := False;
  try
    ls.getDouble(3);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);
end;

procedure TSDODataObjectList_BaseTest.set_double_cursor();
const VAL_1 : TSDODouble = 8877552292000; VAL_2 : TSDODouble = -13654792522; VAL_3 : TSDODouble = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDODoubleType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setDouble(VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(ZERO_DOUBLE);
  c.MoveFirst();
  ls.setDouble(VAL_1);
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PSDODouble( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
    ls.setDouble(VAL_2);
    CheckEquals(VAL_2,PSDODouble( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    ls.setDouble(VAL_1);

  ls.append(ZERO_DOUBLE);
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  ls.setDouble(VAL_2);
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PSDODouble( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PSDODouble( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls.append(ZERO_DOUBLE);
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  CheckEquals(True,c.MoveNext());
  ls.setDouble(VAL_3);
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,PSDODouble( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PSDODouble( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PSDODouble( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDODoubleType.Create(FFactory));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  CheckEquals(True,c.MoveFirst());
    ls.setDouble(0);
  CheckEquals(True,c.MoveNext());
    ls.setDouble(0);
  CheckEquals(True,c.MoveNext());
    ls.setDouble(0);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDODouble( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDODouble( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDODouble( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.set_double_index();
const VAL_1 : TSDODouble = 8877552292000; VAL_2 : TSDODouble = -13654792522; VAL_3 : TSDODouble = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDODoubleType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setDouble(1,VAL_1);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(ZERO_DOUBLE);
  ls.setDouble(0,VAL_1);
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PSDODouble( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
    ls.setDouble(0,VAL_2);
    CheckEquals(VAL_2,PSDODouble( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    ls.setDouble(0,VAL_1);

  ls.append(ZERO_DOUBLE);
  ls.setDouble(1,VAL_2);
  CheckEquals(2,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PSDODouble( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PSDODouble( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls.append(ZERO_DOUBLE);
  ls.setDouble(2,VAL_3);
  CheckEquals(3,ls.size());
    CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,PSDODouble( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PSDODouble( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PSDODouble( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDODoubleType.Create(FFactory));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  ls.setDouble(0,0);
  ls.setDouble(1,0);
  ls.setDouble(2,0);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDODouble( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDODouble( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDODouble( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.delete_double_with_cursor();
const VAL_1 : TSDODouble = 8877552292000; VAL_2 : TSDODouble = -13654792522; VAL_3 : TSDODouble = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDODoubleType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getDouble());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,ls.getDouble());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getDouble());
      CheckEquals(2,ls.size());
      CheckEquals(True,c.MoveNext());
      ls.delete();
        CheckEquals(True,c.MoveFirst());
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getDouble());
        ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveLast());
      CheckEquals(VAL_2,ls.getDouble());
      CheckEquals(2,ls.size());
      ls.delete();
        CheckEquals(1,ls.size());
        CheckEquals(True,c.MoveLast());
        CheckEquals(VAL_1,ls.getDouble());
        ls.delete();
        CheckEquals(0,ls.size());
end;

procedure TSDODataObjectList_BaseTest.delete_double_with_index();
const VAL_1 : TSDODouble = 8877552292000; VAL_2 : TSDODouble = -13654792522; VAL_3 : TSDODouble = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDODoubleType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ok := False;
  try
    ls.delete(123);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    ls.delete(0);
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getDouble(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(1);
      CheckEquals(VAL_1,ls.getDouble(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getDouble(0));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getDouble(0));
        ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    ls.delete(2);
      CheckEquals(VAL_2,ls.getDouble(1));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_1,ls.getDouble(0));
        ls.delete(0);
        CheckEquals(0,ls.size());
end;
{$ENDIF HAS_SDO_DOUBLE}

{$IFDEF HAS_SDO_FLOAT}
const ZERO_FLOAT : TSDOFloat = 0;
procedure TSDODataObjectList_BaseTest.append_float();
var
  vals : array of TSDOFloat;
  c, i, k : PtrInt;
  ls : ISDODataObjectList;
  crs : ILinkedListCursor;
begin
  c := RandomRange(10,100);
  SetLength(vals,c);
  try
    for i := 0 to Pred(c) do
      vals[i] := RandomRange(Low(TSDOInteger),High(TSDOInteger));
    ls := Create_List(TSDOFloatType.Create(FFactory) as ISDOType);
    crs := ls.getCursor();

    for i := 0 to Pred(c) do begin
      CheckEquals(i, ls.size(), 'size() before');
      ls.append(vals[i]);
      CheckEquals(( i + 1 ),ls.size, 'size() after');
      CheckEquals(True, crs.Eof());
      CheckEquals(vals[i],ls.getFloat());
      for k := 0 to i do begin
        CheckEquals(vals[k],ls.getFloat(k), Format('ls.getFloat(%d)',[k]));
      end;
    end;
  finally
    SetLength(vals,0);
  end;
end;

procedure TSDODataObjectList_BaseTest.insert_float();
var
  ls : ISDODataObjectList;

  procedure CheckInvalidIndex(const AIndex : PtrInt; const AValue : TSDOFloat);
  var
    pass : Boolean;
    oldSize : PtrInt;
  begin
    oldSize := ls.size();
    pass := False;
    try
      ls.insert(-10,AValue);
    except
      on e : ESDOIndexOutOfRangeException do begin
        pass := True;
      end;
    end;
    CheckEquals(True,pass, Format('Index = %d',[AIndex]));
    CheckEquals(oldSize, ls.size(), 'A failed "insert" must not modify the list''s size.');
  end;

var
  c : ILinkedListCursor;
  val_1, val_2, val_3 : TSDOFloat;
begin
  val_1 := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  val_2 := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  val_3 := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  ls := Create_List(TSDOFloatType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  CheckInvalidIndex(-12, val_1);
  CheckInvalidIndex(-1, val_1);
  CheckInvalidIndex(1, val_1);
  CheckInvalidIndex(12, val_1);

  ls.insert(0,val_1);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_1, ls.getFloat(0));
    ls.insert(1,val_2);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getFloat(0));
      CheckEquals(val_2, ls.getFloat(1));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getFloat(0));
        CheckEquals(val_2, ls.getFloat(1));
        CheckEquals(val_3, ls.getFloat(2));

  ls := Create_List(TSDOFloatType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insert(0,val_2);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_2, ls.getFloat(0));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getFloat(0));
      CheckEquals(val_2, ls.getFloat(1));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getFloat(0));
        CheckEquals(val_2, ls.getFloat(1));
        CheckEquals(val_3, ls.getFloat(2));

  ls := Create_List(TSDOFloatType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insert(0,val_3);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_3, ls.getFloat(0));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getFloat(0));
      CheckEquals(val_3, ls.getFloat(1));
      ls.insert(1,val_2);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getFloat(0));
        CheckEquals(val_2, ls.getFloat(1));
        CheckEquals(val_3, ls.getFloat(2));
end;

procedure TSDODataObjectList_BaseTest.get_float_cursor();
const VAL_1 : TSDOFloat = 8877552292000; VAL_2 : TSDOFloat = -13654792522; VAL_3 : TSDOFloat = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOFloatType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getFloat();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(ZERO_FLOAT);
  Check(c.MoveLast());
  PSDOFloat(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;
  CheckEquals(1,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getFloat());
    CheckEquals(False,c.MoveNext());
    PSDOFloat(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(VAL_2,ls.getFloat());
    PSDOFloat(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;

  ls.append(ZERO_FLOAT);
  Check(c.MoveLast());
  PSDOFloat(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getFloat());
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,ls.getFloat());
    CheckEquals(False,c.MoveNext());

  ls.append(ZERO_FLOAT);
  Check(c.MoveLast());
  PSDOFloat(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_3;
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,ls.getFloat());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,ls.getFloat());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,ls.getFloat());
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOFloatType.Create(FFactory));
  c := ls.getCursor();
  ls.append(ZERO_FLOAT);
  ls.append(ZERO_FLOAT);
  ls.append(ZERO_FLOAT);
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,ls.getFloat());
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,ls.getFloat());
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,ls.getFloat());
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.get_float_index();
const VAL_1 : TSDOFloat = 8877552292000; VAL_2 : TSDOFloat = -13654792522; VAL_3 : TSDOFloat = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOFloatType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getFloat(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(ZERO_FLOAT);
  Check(c.MoveLast());
  PSDOFloat(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;
  CheckEquals(1,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,ls.getFloat(0));
    PSDOFloat(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(VAL_2,ls.getFloat(0));
    PSDOFloat(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;

  ls.append(ZERO_FLOAT);
  Check(c.MoveLast());
  PSDOFloat(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(2,ls.size());
    CheckEquals(VAL_1,ls.getFloat(0));
    CheckEquals(VAL_2,ls.getFloat(1));

  ls.append(ZERO_FLOAT);
  Check(c.MoveLast());
  PSDOFloat(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_3;
      CheckEquals(3,ls.size());
      CheckEquals(VAL_1,ls.getFloat(0));
      CheckEquals(VAL_2,ls.getFloat(1));
      CheckEquals(VAL_3,ls.getFloat(2));

  ls := Create_List(TSDOFloatType.Create(FFactory));
  c := ls.getCursor();
  ls.append(ZERO_FLOAT);
  ls.append(ZERO_FLOAT);
  ls.append(ZERO_FLOAT);
    CheckEquals(3,ls.size());
    CheckEquals(0,ls.getFloat(0));
    CheckEquals(0,ls.getFloat(1));
    CheckEquals(0,ls.getFloat(2));

  ok := False;
  try
    ls.getFloat(3);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);
end;

procedure TSDODataObjectList_BaseTest.set_float_cursor();
const VAL_1 : TSDOFloat = 8877552292000; VAL_2 : TSDOFloat = -13654792522; VAL_3 : TSDOFloat = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOFloatType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setFloat(VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(ZERO_FLOAT);
  c.MoveFirst();
  ls.setFloat(VAL_1);
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PSDOFloat( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
    ls.setFloat(VAL_2);
    CheckEquals(VAL_2,PSDOFloat( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    ls.setFloat(VAL_1);

  ls.append(ZERO_FLOAT);
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  ls.setFloat(VAL_2);
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PSDOFloat( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PSDOFloat( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls.append(ZERO_FLOAT);
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  CheckEquals(True,c.MoveNext());
  ls.setFloat(VAL_3);
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,PSDOFloat( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PSDOFloat( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PSDOFloat( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOFloatType.Create(FFactory));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  CheckEquals(True,c.MoveFirst());
    ls.setFloat(0);
  CheckEquals(True,c.MoveNext());
    ls.setFloat(0);
  CheckEquals(True,c.MoveNext());
    ls.setFloat(0);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOFloat( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOFloat( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOFloat( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.set_float_index();
const VAL_1 : TSDOFloat = 8877552292000; VAL_2 : TSDOFloat = -13654792522; VAL_3 : TSDOFloat = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOFloatType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setFloat(1,VAL_1);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(ZERO_FLOAT);
  ls.setFloat(0,VAL_1);
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PSDOFloat( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
    ls.setFloat(0,VAL_2);
    CheckEquals(VAL_2,PSDOFloat( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    ls.setFloat(0,VAL_1);

  ls.append(ZERO_FLOAT);
  ls.setFloat(1,VAL_2);
  CheckEquals(2,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PSDOFloat( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PSDOFloat( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls.append(ZERO_FLOAT);
  ls.setFloat(2,VAL_3);
  CheckEquals(3,ls.size());
    CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,PSDOFloat( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PSDOFloat( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PSDOFloat( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOFloatType.Create(FFactory));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  ls.setFloat(0,0);
  ls.setFloat(1,0);
  ls.setFloat(2,0);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOFloat( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOFloat( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOFloat( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.delete_float_with_cursor();
const VAL_1 : TSDOFloat = 8877552292000; VAL_2 : TSDOFloat = -13654792522; VAL_3 : TSDOFloat = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOFloatType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getFloat());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,ls.getFloat());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getFloat());
      CheckEquals(2,ls.size());
      CheckEquals(True,c.MoveNext());
      ls.delete();
        CheckEquals(True,c.MoveFirst());
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getFloat());
        ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveLast());
      CheckEquals(VAL_2,ls.getFloat());
      CheckEquals(2,ls.size());
      ls.delete();
        CheckEquals(1,ls.size());
        CheckEquals(True,c.MoveLast());
        CheckEquals(VAL_1,ls.getFloat());
        ls.delete();
        CheckEquals(0,ls.size());
end;

procedure TSDODataObjectList_BaseTest.delete_float_with_index();
const VAL_1 : TSDOFloat = 8877552292000; VAL_2 : TSDOFloat = -13654792522; VAL_3 : TSDOFloat = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOFloatType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ok := False;
  try
    ls.delete(123);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    ls.delete(0);
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getFloat(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(1);
      CheckEquals(VAL_1,ls.getFloat(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getFloat(0));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getFloat(0));
        ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    ls.delete(2);
      CheckEquals(VAL_2,ls.getFloat(1));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_1,ls.getFloat(0));
        ls.delete(0);
        CheckEquals(0,ls.size());
end;
{$ENDIF HAS_SDO_FLOAT}

{$IFDEF HAS_SDO_LONG}
procedure TSDODataObjectList_BaseTest.append_long();
var
  vals : array of TSDOLong;
  c, i, k : PtrInt;
  ls : ISDODataObjectList;
  crs : ILinkedListCursor;
begin
  c := RandomRange(10,100);
  SetLength(vals,c);
  try
    for i := 0 to Pred(c) do
      vals[i] := RandomRange(Low(TSDOInteger),High(TSDOInteger));
    ls := Create_List(TSDOLongType.Create(FFactory) as ISDOType);
    crs := ls.getCursor();

    for i := 0 to Pred(c) do begin
      CheckEquals(i, ls.size(), 'size() before');
      ls.append(vals[i]);
      CheckEquals(( i + 1 ),ls.size, 'size() after');
      CheckEquals(True, crs.Eof());
      CheckEquals(vals[i],ls.getLong());
      for k := 0 to i do begin
        CheckEquals(vals[k],ls.getLong(k), Format('ls.getLong(%d)',[k]));
      end;
    end;
  finally
    SetLength(vals,0);
  end;
end;

procedure TSDODataObjectList_BaseTest.insert_long();
var
  ls : ISDODataObjectList;

  procedure CheckInvalidIndex(const AIndex : PtrInt; const AValue : TSDOLong);
  var
    pass : Boolean;
    oldSize : PtrInt;
  begin
    oldSize := ls.size();
    pass := False;
    try
      ls.insert(-10,AValue);
    except
      on e : ESDOIndexOutOfRangeException do begin
        pass := True;
      end;
    end;
    CheckEquals(True,pass, Format('Index = %d',[AIndex]));
    CheckEquals(oldSize, ls.size(), 'A failed "insert" must not modify the list''s size.');
  end;

var
  c : ILinkedListCursor;
  val_1, val_2, val_3 : TSDOLong;
begin
  val_1 := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  val_2 := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  val_3 := RandomRange(Low(TSDOInteger),High(TSDOInteger));
  ls := Create_List(TSDOLongType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  CheckInvalidIndex(-12, val_1);
  CheckInvalidIndex(-1, val_1);
  CheckInvalidIndex(1, val_1);
  CheckInvalidIndex(12, val_1);

  ls.insert(0,val_1);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_1, ls.getLong(0));
    ls.insert(1,val_2);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getLong(0));
      CheckEquals(val_2, ls.getLong(1));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getLong(0));
        CheckEquals(val_2, ls.getLong(1));
        CheckEquals(val_3, ls.getLong(2));

  ls := Create_List(TSDOLongType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insert(0,val_2);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_2, ls.getLong(0));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getLong(0));
      CheckEquals(val_2, ls.getLong(1));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getLong(0));
        CheckEquals(val_2, ls.getLong(1));
        CheckEquals(val_3, ls.getLong(2));

  ls := Create_List(TSDOLongType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insert(0,val_3);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_3, ls.getLong(0));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getLong(0));
      CheckEquals(val_3, ls.getLong(1));
      ls.insert(1,val_2);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getLong(0));
        CheckEquals(val_2, ls.getLong(1));
        CheckEquals(val_3, ls.getLong(2));
end;

procedure TSDODataObjectList_BaseTest.get_long_cursor();
const VAL_1 : TSDOLong = 8877552292000; VAL_2 : TSDOLong = -13654792522; VAL_3 : TSDOLong = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOLongType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getLong();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(TSDOLong(0));
  Check(c.MoveLast());
  PSDOLong(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;
  CheckEquals(1,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getLong());
    CheckEquals(False,c.MoveNext());
    PSDOLong(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(VAL_2,ls.getLong());
    PSDOLong(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;

  ls.append(TSDOLong(0));
  Check(c.MoveLast());
  PSDOLong(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getLong());
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,ls.getLong());
    CheckEquals(False,c.MoveNext());

  ls.append(TSDOLong(0));
  Check(c.MoveLast());
  PSDOLong(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_3;
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,ls.getLong());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,ls.getLong());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,ls.getLong());
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOLongType.Create(FFactory));
  c := ls.getCursor();
  ls.append(TSDOLong(0));
  ls.append(TSDOLong(0));
  ls.append(TSDOLong(0));
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,ls.getLong());
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,ls.getLong());
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,ls.getLong());
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.get_long_index();
const VAL_1 : TSDOLong = 8877552292000; VAL_2 : TSDOLong = -13654792522; VAL_3 : TSDOLong = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOLongType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getLong(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(TSDOLong(0));
  Check(c.MoveLast());
  PSDOLong(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;
  CheckEquals(1,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,ls.getLong(0));
    PSDOLong(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(VAL_2,ls.getLong(0));
    PSDOLong(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;

  ls.append(TSDOLong(0));
  Check(c.MoveLast());
  PSDOLong(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(2,ls.size());
    CheckEquals(VAL_1,ls.getLong(0));
    CheckEquals(VAL_2,ls.getLong(1));

  ls.append(TSDOLong(0));
  Check(c.MoveLast());
  PSDOLong(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_3;
      CheckEquals(3,ls.size());
      CheckEquals(VAL_1,ls.getLong(0));
      CheckEquals(VAL_2,ls.getLong(1));
      CheckEquals(VAL_3,ls.getLong(2));

  ls := Create_List(TSDOLongType.Create(FFactory));
  c := ls.getCursor();
  ls.append(TSDOLong(0));
  ls.append(TSDOLong(0));
  ls.append(TSDOLong(0));
    CheckEquals(3,ls.size());
    CheckEquals(0,ls.getLong(0));
    CheckEquals(0,ls.getLong(1));
    CheckEquals(0,ls.getLong(2));

  ok := False;
  try
    ls.getLong(3);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);
end;

procedure TSDODataObjectList_BaseTest.set_long_cursor();
const VAL_1 : TSDOLong = 8877552292000; VAL_2 : TSDOLong = -13654792522; VAL_3 : TSDOLong = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOLongType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setLong(VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(TSDOLong(0));
  c.MoveFirst();
  ls.setLong(VAL_1);
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PSDOLong( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
    ls.setLong(VAL_2);
    CheckEquals(VAL_2,PSDOLong( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    ls.setLong(VAL_1);

  ls.append(TSDOLong(0));
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  ls.setLong(VAL_2);
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PSDOLong( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PSDOLong( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls.append(TSDOLong(0));
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  CheckEquals(True,c.MoveNext());
  ls.setLong(VAL_3);
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,PSDOLong( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PSDOLong( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PSDOLong( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOLongType.Create(FFactory));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  CheckEquals(True,c.MoveFirst());
    ls.setLong(0);
  CheckEquals(True,c.MoveNext());
    ls.setLong(0);
  CheckEquals(True,c.MoveNext());
    ls.setLong(0);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOLong( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOLong( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOLong( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.set_long_index();
const VAL_1 : TSDOLong = 8877552292000; VAL_2 : TSDOLong = -13654792522; VAL_3 : TSDOLong = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOLongType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setLong(1,VAL_1);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(TSDOLong(0));
  ls.setLong(0,VAL_1);
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PSDOLong( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
    ls.setLong(0,VAL_2);
    CheckEquals(VAL_2,PSDOLong( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    ls.setLong(0,VAL_1);

  ls.append(TSDOLong(0));
  ls.setLong(1,VAL_2);
  CheckEquals(2,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PSDOLong( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PSDOLong( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls.append(TSDOLong(0));
  ls.setLong(2,VAL_3);
  CheckEquals(3,ls.size());
    CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,PSDOLong( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PSDOLong( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PSDOLong( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOLongType.Create(FFactory));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  ls.setLong(0,0);
  ls.setLong(1,0);
  ls.setLong(2,0);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOLong( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOLong( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOLong( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.delete_long_with_cursor();
const VAL_1 : TSDOLong = 8877552292000; VAL_2 : TSDOLong = -13654792522; VAL_3 : TSDOLong = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOLongType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getLong());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,ls.getLong());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getLong());
      CheckEquals(2,ls.size());
      CheckEquals(True,c.MoveNext());
      ls.delete();
        CheckEquals(True,c.MoveFirst());
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getLong());
        ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveLast());
      CheckEquals(VAL_2,ls.getLong());
      CheckEquals(2,ls.size());
      ls.delete();
        CheckEquals(1,ls.size());
        CheckEquals(True,c.MoveLast());
        CheckEquals(VAL_1,ls.getLong());
        ls.delete();
        CheckEquals(0,ls.size());
end;

procedure TSDODataObjectList_BaseTest.delete_long_with_index();
const VAL_1 : TSDOLong = 8877552292000; VAL_2 : TSDOLong = -13654792522; VAL_3 : TSDOLong = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOLongType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ok := False;
  try
    ls.delete(123);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    ls.delete(0);
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getLong(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(1);
      CheckEquals(VAL_1,ls.getLong(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getLong(0));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getLong(0));
        ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    ls.delete(2);
      CheckEquals(VAL_2,ls.getLong(1));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_1,ls.getLong(0));
        ls.delete(0);
        CheckEquals(0,ls.size());
end;
{$ENDIF HAS_SDO_LONG}

{$IFDEF HAS_SDO_SHORT}
procedure TSDODataObjectList_BaseTest.append_short();
var
  vals : array of TSDOShort;
  c, i, k : PtrInt;
  ls : ISDODataObjectList;
  crs : ILinkedListCursor;
begin
  c := RandomRange(10,100);
  SetLength(vals,c);
  try
    for i := 0 to Pred(c) do
      vals[i] := RandomRange(Low(TSDOShort),High(TSDOShort));
    ls := Create_List(TSDOShortType.Create(FFactory) as ISDOType);
    crs := ls.getCursor();

    for i := 0 to Pred(c) do begin
      CheckEquals(i, ls.size(), 'size() before');
      ls.append(vals[i]);
      CheckEquals(( i + 1 ),ls.size, 'size() after');
      CheckEquals(True, crs.Eof());
      CheckEquals(vals[i],ls.getShort());
      for k := 0 to i do begin
        CheckEquals(vals[k],ls.getShort(k), Format('ls.getShort(%d)',[k]));
      end;
    end;
  finally
    SetLength(vals,0);
  end;
end;

procedure TSDODataObjectList_BaseTest.insert_short();
var
  ls : ISDODataObjectList;

  procedure CheckInvalidIndex(const AIndex : PtrInt; const AValue : TSDOShort);
  var
    pass : Boolean;
    oldSize : PtrInt;
  begin
    oldSize := ls.size();
    pass := False;
    try
      ls.insert(-10,AValue);
    except
      on e : ESDOIndexOutOfRangeException do begin
        pass := True;
      end;
    end;
    CheckEquals(True,pass, Format('Index = %d',[AIndex]));
    CheckEquals(oldSize, ls.size(), 'A failed "insert" must not modify the list''s size.');
  end;

var
  c : ILinkedListCursor;
  val_1, val_2, val_3 : TSDOShort;
begin
  val_1 := RandomRange(Low(TSDOShort),High(TSDOShort));
  val_2 := RandomRange(Low(TSDOShort),High(TSDOShort));
  val_3 := RandomRange(Low(TSDOShort),High(TSDOShort));
  ls := Create_List(TSDOShortType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  CheckInvalidIndex(-12, val_1);
  CheckInvalidIndex(-1, val_1);
  CheckInvalidIndex(1, val_1);
  CheckInvalidIndex(12, val_1);

  ls.insert(0,val_1);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_1, ls.getShort(0));
    ls.insert(1,val_2);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getShort(0));
      CheckEquals(val_2, ls.getShort(1));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getShort(0));
        CheckEquals(val_2, ls.getShort(1));
        CheckEquals(val_3, ls.getShort(2));

  ls := Create_List(TSDOShortType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insert(0,val_2);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_2, ls.getShort(0));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getShort(0));
      CheckEquals(val_2, ls.getShort(1));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getShort(0));
        CheckEquals(val_2, ls.getShort(1));
        CheckEquals(val_3, ls.getShort(2));

  ls := Create_List(TSDOShortType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insert(0,val_3);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_3, ls.getShort(0));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getShort(0));
      CheckEquals(val_3, ls.getShort(1));
      ls.insert(1,val_2);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getShort(0));
        CheckEquals(val_2, ls.getShort(1));
        CheckEquals(val_3, ls.getShort(2));
end;

procedure TSDODataObjectList_BaseTest.get_short_cursor();
const VAL_1 : TSDOShort = 4567; VAL_2 : TSDOShort = -9876; VAL_3 : TSDOShort = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOShortType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getShort();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(TSDOShort(0));
  Check(c.MoveLast());
  PSDOShort(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;
  CheckEquals(1,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getShort());
    CheckEquals(False,c.MoveNext());
    PSDOShort(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(VAL_2,ls.getShort());
    PSDOShort(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;

  ls.append(TSDOShort(0));
  Check(c.MoveLast());
  PSDOShort(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getShort());
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,ls.getShort());
    CheckEquals(False,c.MoveNext());

  ls.append(TSDOShort(0));
  Check(c.MoveLast());
  PSDOShort(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_3;
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,ls.getShort());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,ls.getShort());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,ls.getShort());
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOShortType.Create(FFactory));
  c := ls.getCursor();
  ls.append(TSDOShort(0));
  ls.append(TSDOShort(0));
  ls.append(TSDOShort(0));
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,ls.getShort());
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,ls.getShort());
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,ls.getShort());
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.get_short_index();
const VAL_1 : TSDOShort = 4567; VAL_2 : TSDOShort = -9876; VAL_3 : TSDOShort = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOShortType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getShort(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(TSDOShort(0));
  Check(c.MoveLast());
  PSDOShort(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;
  CheckEquals(1,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,ls.getShort(0));
    PSDOShort(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(VAL_2,ls.getShort(0));
    PSDOShort(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;

  ls.append(TSDOShort(0));
  Check(c.MoveLast());
  PSDOShort(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(2,ls.size());
    CheckEquals(VAL_1,ls.getShort(0));
    CheckEquals(VAL_2,ls.getShort(1));

  ls.append(TSDOShort(0));
  Check(c.MoveLast());
  PSDOShort(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_3;
      CheckEquals(3,ls.size());
      CheckEquals(VAL_1,ls.getShort(0));
      CheckEquals(VAL_2,ls.getShort(1));
      CheckEquals(VAL_3,ls.getShort(2));

  ls := Create_List(TSDOShortType.Create(FFactory));
  c := ls.getCursor();
  ls.append(TSDOShort(0));
  ls.append(TSDOShort(0));
  ls.append(TSDOShort(0));
    CheckEquals(3,ls.size());
    CheckEquals(0,ls.getShort(0));
    CheckEquals(0,ls.getShort(1));
    CheckEquals(0,ls.getShort(2));

  ok := False;
  try
    ls.getShort(3);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);
end;

procedure TSDODataObjectList_BaseTest.set_short_cursor();
const VAL_1 : TSDOShort = 4567; VAL_2 : TSDOShort = -9876; VAL_3 : TSDOShort = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOShortType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setShort(VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(TSDOShort(0));
  c.MoveFirst();
  ls.setShort(VAL_1);
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PSDOShort( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
    ls.setShort(VAL_2);
    CheckEquals(VAL_2,PSDOShort( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    ls.setShort(VAL_1);

  ls.append(TSDOShort(0));
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  ls.setShort(VAL_2);
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PSDOShort( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PSDOShort( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls.append(TSDOShort(0));
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  CheckEquals(True,c.MoveNext());
  ls.setShort(VAL_3);
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,PSDOShort( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PSDOShort( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PSDOShort( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOShortType.Create(FFactory));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  CheckEquals(True,c.MoveFirst());
    ls.setShort(0);
  CheckEquals(True,c.MoveNext());
    ls.setShort(0);
  CheckEquals(True,c.MoveNext());
    ls.setShort(0);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOShort( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOShort( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOShort( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.set_short_index();
const VAL_1 : TSDOShort = 4567; VAL_2 : TSDOShort = -9876; VAL_3 : TSDOShort = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOShortType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setShort(1,VAL_1);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(TSDOShort(0));
  ls.setShort(0,VAL_1);
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PSDOShort( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
    ls.setShort(0,VAL_2);
    CheckEquals(VAL_2,PSDOShort( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    ls.setShort(0,VAL_1);

  ls.append(TSDOShort(0));
  ls.setShort(1,VAL_2);
  CheckEquals(2,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PSDOShort( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PSDOShort( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls.append(TSDOShort(0));
  ls.setShort(2,VAL_3);
  CheckEquals(3,ls.size());
    CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,PSDOShort( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PSDOShort( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PSDOShort( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOShortType.Create(FFactory));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  ls.setShort(0,0);
  ls.setShort(1,0);
  ls.setShort(2,0);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOShort( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOShort( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOShort( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.delete_short_with_cursor();
const VAL_1 : TSDOShort = 4567; VAL_2 : TSDOShort = -9876; VAL_3 : TSDOShort = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOShortType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getShort());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,ls.getShort());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getShort());
      CheckEquals(2,ls.size());
      CheckEquals(True,c.MoveNext());
      ls.delete();
        CheckEquals(True,c.MoveFirst());
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getShort());
        ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveLast());
      CheckEquals(VAL_2,ls.getShort());
      CheckEquals(2,ls.size());
      ls.delete();
        CheckEquals(1,ls.size());
        CheckEquals(True,c.MoveLast());
        CheckEquals(VAL_1,ls.getShort());
        ls.delete();
        CheckEquals(0,ls.size());
end;

procedure TSDODataObjectList_BaseTest.delete_short_with_index();
const VAL_1 : TSDOShort = 4567; VAL_2 : TSDOShort = -9876; VAL_3 : TSDOShort = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOShortType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ok := False;
  try
    ls.delete(123);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    ls.delete(0);
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getShort(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(1);
      CheckEquals(VAL_1,ls.getShort(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getShort(0));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getShort(0));
        ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    ls.delete(2);
      CheckEquals(VAL_2,ls.getShort(1));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_1,ls.getShort(0));
        ls.delete(0);
        CheckEquals(0,ls.size());
end;
{$ENDIF HAS_SDO_SHORT}

procedure TSDODataObjectList_BaseTest.CheckEquals(expected,
  actual: TSDODate; msg: string; const AStrict: Boolean);
var
  e, a : TDateTime;
  e_y, e_m, e_d, e_h, e_mn, e_ss, e_ms : Word;
  a_y, a_m, a_d, a_h, a_mn, a_ss, a_ms : Word;
begin
  if AStrict then begin
    Check(CompareMem(@expected, @actual, SizeOf(TSDODate)), msg);
  end else begin
    e := NormalizeToUTC(expected);
    a := NormalizeToUTC(actual);
    DecodeDateTime(e, e_y, e_m, e_d, e_h, e_mn, e_ss, e_ms);
    DecodeDateTime(a, a_y, a_m, a_d, a_h, a_mn, a_ss, a_ms);
    CheckEquals(e_y,a_y,msg);
    CheckEquals(e_m,a_m,msg);
    CheckEquals(e_d,a_d,msg);
    CheckEquals(e_h,a_h,msg);
    CheckEquals(e_mn,a_mn,msg);
    CheckEquals(e_ss,a_ss,msg);
    CheckEquals(e_ms,a_ms,msg);
  end;
end;

class function TSDODataObjectList_BaseTest.Create_Factory: ISDODataFactory;
var
  typ : ISDOType;
begin
  Result := TSDODataFactory.Create();
  Result.AddType(s_URI_1,s_TYPE_1,[]);
  typ := Result.getType(s_URI_1,s_TYPE_1);
    Result.addProperty(typ,s_PROP_BOOL_1,sdo_namespace,'Boolean',[]);
    Result.addProperty(typ,s_PROP_BYTE_1,sdo_namespace,'Byte',[]);
    Result.addProperty(typ,s_PROP_INTEGER_1,sdo_namespace,'Integer',[]);
    Result.addProperty(typ,s_PROP_STR_1,sdo_namespace,'String',[]);
end;

function TSDODataObjectList_BaseTest.Create_Object() : ISDODataObject;
begin
  Result := FFactory.createNew(s_URI_1,s_TYPE_1);
end;

procedure TSDODataObjectList_BaseTest.delete_boolean_with_cursor();
const VAL_1 = False; VAL_2 = True; VAL_3 = False;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOBooleanType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getBoolean());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,ls.getBoolean());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getBoolean());
      CheckEquals(2,ls.size());
      CheckEquals(True,c.MoveNext());
      ls.delete();
        CheckEquals(True,c.MoveFirst());
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getBoolean());
        ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveLast());
      CheckEquals(VAL_2,ls.getBoolean());
      CheckEquals(2,ls.size());
      ls.delete();
        CheckEquals(1,ls.size());
        CheckEquals(True,c.MoveLast());
        CheckEquals(VAL_1,ls.getBoolean());
        ls.delete();
        CheckEquals(0,ls.size());
end;

procedure TSDODataObjectList_BaseTest.delete_boolean_with_index();
const VAL_1 = True; VAL_2 = False; VAL_3 = True;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOBooleanType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ok := False;
  try
    ls.delete(123);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    ls.delete(0);
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getBoolean(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(1);
      CheckEquals(VAL_1,ls.getBoolean(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getBoolean(0));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getBoolean(0));
        ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    ls.delete(2);
      CheckEquals(VAL_2,ls.getBoolean(1));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_1,ls.getBoolean(0));
        ls.delete(0);
        CheckEquals(0,ls.size());
end;

procedure TSDODataObjectList_BaseTest.delete_byte_with_cursor();
const VAL_1 : TSDOByte = 12; VAL_2 : TSDOByte = 34; VAL_3 : TSDOByte = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOByteType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getByte());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,ls.getByte());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getByte());
      CheckEquals(2,ls.size());
      CheckEquals(True,c.MoveNext());
      ls.delete();
        CheckEquals(True,c.MoveFirst());
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getByte());
        ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveLast());
      CheckEquals(VAL_2,ls.getByte());
      CheckEquals(2,ls.size());
      ls.delete();
        CheckEquals(1,ls.size());
        CheckEquals(True,c.MoveLast());
        CheckEquals(VAL_1,ls.getByte());
        ls.delete();
        CheckEquals(0,ls.size());
end;

procedure TSDODataObjectList_BaseTest.delete_byte_with_index();
const VAL_1 : TSDOByte = 12; VAL_2 : TSDOByte = 34; VAL_3 : TSDOByte = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOByteType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ok := False;
  try
    ls.delete(123);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    ls.delete(0);
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getByte(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(1);
      CheckEquals(VAL_1,ls.getByte(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getByte(0));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getByte(0));
        ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    ls.delete(2);
      CheckEquals(VAL_2,ls.getByte(1));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_1,ls.getByte(0));
        ls.delete(0);
        CheckEquals(0,ls.size());
end;

procedure TSDODataObjectList_BaseTest.delete_dataObject_with_cursor();
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
  VAL_1, VAL_2, VAL_3 : ISDODataObject;
begin
  VAL_1 := FFactory.createNew(s_URI_1,s_TYPE_1);
  VAL_2 := FFactory.createNew(s_URI_1,s_TYPE_1);
  VAL_3 := FFactory.createNew(s_URI_1,s_TYPE_1);

  ls := Create_List(FFactory.getType(s_URI_1,s_TYPE_1));
  c := ls.getCursor();

  ok := False;
  try
    ls.delete();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(PTrUInt(VAL_2),PTrUInt(ls.getDataObject()));
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(PTrUInt(VAL_1),PTrUInt(ls.getDataObject()));
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(PTrUInt(VAL_2),PTrUInt(ls.getDataObject()));
      CheckEquals(2,ls.size());
      CheckEquals(True,c.MoveNext());
      ls.delete();
        CheckEquals(True,c.MoveFirst());
        CheckEquals(1,ls.size());
        CheckEquals(PTrUInt(VAL_2),PTrUInt(ls.getDataObject()));
        ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveLast());
      CheckEquals(PTrUInt(VAL_2),PTrUInt(ls.getDataObject()));
      CheckEquals(2,ls.size());
      ls.delete();
        CheckEquals(1,ls.size());
        CheckEquals(True,c.MoveLast());
        CheckEquals(PTrUInt(VAL_1),PTrUInt(ls.getDataObject()));
        ls.delete();
        CheckEquals(0,ls.size());
end;

procedure TSDODataObjectList_BaseTest.delete_dataObject_with_index();
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
  VAL_1, VAL_2, VAL_3 : ISDODataObject;
begin
  VAL_1 := FFactory.createNew(s_URI_1,s_TYPE_1);
  VAL_2 := FFactory.createNew(s_URI_1,s_TYPE_1);
  VAL_3 := FFactory.createNew(s_URI_1,s_TYPE_1);

  ls := Create_List(FFactory.getType(s_URI_1,s_TYPE_1));
  c := ls.getCursor();

  ok := False;
  try
    ls.delete(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ok := False;
  try
    ls.delete(123);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    ls.delete(0);
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(0);
      CheckEquals(PtrUInt(VAL_2),PtrUInt(ls.getDataObject(0)));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(1);
      CheckEquals(PtrUInt(VAL_1),PtrUInt(ls.getDataObject(0)));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    ls.delete(0);
      CheckEquals(PtrUInt(VAL_2),PtrUInt(ls.getDataObject(0)));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(PtrUInt(VAL_2),PtrUInt(ls.getDataObject(0)));
        ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    ls.delete(2);
      CheckEquals(PtrUInt(VAL_2),PtrUInt(ls.getDataObject(1)));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(PtrUInt(VAL_1),PtrUInt(ls.getDataObject(0)));
        ls.delete(0);
        CheckEquals(0,ls.size());
end;

procedure TSDODataObjectList_BaseTest.delete_date_with_cursor();
const VAL_1 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_2 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_3 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );

  procedure SetConstants();
  var
    d : TSDODate;
  begin
    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(1976,10,12,23,34,45,56);
    d.HourOffset := 5;
    d.MinuteOffset := 6;
    PSDODate(@VAL_1)^ := d;

    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(2008,7,8,9,10,11,12);
    d.HourOffset := 0;
    d.MinuteOffset := 13;
    PSDODate(@VAL_2)^ := d;
  end;

var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  SetConstants();
  ls := Create_List(TSDODateTimeType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getDate());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,ls.getDate());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getDate());
      CheckEquals(2,ls.size());
      CheckEquals(True,c.MoveNext());
      ls.delete();
        CheckEquals(True,c.MoveFirst());
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getDate());
        ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveLast());
      CheckEquals(VAL_2,ls.getDate());
      CheckEquals(2,ls.size());
      ls.delete();
        CheckEquals(1,ls.size());
        CheckEquals(True,c.MoveLast());
        CheckEquals(VAL_1,ls.getDate());
        ls.delete();
        CheckEquals(0,ls.size());
end;

procedure TSDODataObjectList_BaseTest.delete_date_with_index();
const VAL_1 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_2 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_3 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );

  procedure SetConstants();
  var
    d : TSDODate;
  begin
    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(1976,10,12,23,34,45,56);
    d.HourOffset := 5;
    d.MinuteOffset := 6;
    PSDODate(@VAL_1)^ := d;

    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(2008,7,8,9,10,11,12);
    d.HourOffset := 0;
    d.MinuteOffset := 13;
    PSDODate(@VAL_2)^ := d;
  end;

var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  SetConstants();
  ls := Create_List(TSDODateTimeType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ok := False;
  try
    ls.delete(123);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    ls.delete(0);
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getDate(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(1);
      CheckEquals(VAL_1,ls.getDate(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getDate(0));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getDate(0));
        ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    ls.delete(2);
      CheckEquals(VAL_2,ls.getDate(1));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_1,ls.getDate(0));
        ls.delete(0);
        CheckEquals(0,ls.size());
end;

procedure TSDODataObjectList_BaseTest.delete_integer_with_cursor();
const VAL_1 : TSDOInteger = 1210; VAL_2 : TSDOInteger = -97456; VAL_3 : TSDOInteger = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOIntegerType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getInteger());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,ls.getInteger());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getInteger());
      CheckEquals(2,ls.size());
      CheckEquals(True,c.MoveNext());
      ls.delete();
        CheckEquals(True,c.MoveFirst());
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getInteger());
        ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveLast());
      CheckEquals(VAL_2,ls.getInteger());
      CheckEquals(2,ls.size());
      ls.delete();
        CheckEquals(1,ls.size());
        CheckEquals(True,c.MoveLast());
        CheckEquals(VAL_1,ls.getInteger());
        ls.delete();
        CheckEquals(0,ls.size());
end;

procedure TSDODataObjectList_BaseTest.delete_integer_with_index();
const VAL_1 : TSDOInteger = 1210; VAL_2 : TSDOInteger = -97456; VAL_3 : TSDOInteger = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOIntegerType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ok := False;
  try
    ls.delete(123);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    ls.delete(0);
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getInteger(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(1);
      CheckEquals(VAL_1,ls.getInteger(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getInteger(0));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getInteger(0));
        ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    ls.delete(2);
      CheckEquals(VAL_2,ls.getInteger(1));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_1,ls.getInteger(0));
        ls.delete(0);
        CheckEquals(0,ls.size());
end;

procedure TSDODataObjectList_BaseTest.delete_string_with_cursor();
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
  VAL_1, VAL_2, VAL_3 : string;
begin
  Randomize();
  VAL_1 := RandomString(RandomRange(0,1000));
  VAL_2 := RandomString(RandomRange(0,1000));
  VAL_3 := RandomString(RandomRange(0,1000));
  ls := Create_List(TSDOStringType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getString());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,ls.getString());
      CheckEquals(1,ls.size());
      ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveNext());
    ls.delete();
      CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_2,ls.getString());
      CheckEquals(2,ls.size());
      CheckEquals(True,c.MoveNext());
      ls.delete();
        CheckEquals(True,c.MoveFirst());
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getString());
        ls.delete();
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    CheckEquals(True,c.MoveLast());
    ls.delete();
      CheckEquals(True,c.MoveLast());
      CheckEquals(VAL_2,ls.getString());
      CheckEquals(2,ls.size());
      ls.delete();
        CheckEquals(1,ls.size());
        CheckEquals(True,c.MoveLast());
        CheckEquals(VAL_1,ls.getString());
        ls.delete();
        CheckEquals(0,ls.size());
end;

procedure TSDODataObjectList_BaseTest.delete_string_with_index();
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
  VAL_1, VAL_2, VAL_3 : string;
begin
  Randomize();
  VAL_1 := RandomString(RandomRange(0,1000));
  VAL_2 := RandomString(RandomRange(0,1000));
  VAL_3 := RandomString(RandomRange(0,1000));
  ls := Create_List(TSDOStringType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  ok := False;
  try
    ls.delete(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ok := False;
  try
    ls.delete(123);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(VAL_1);
    ls.delete(0);
    CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getString(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
    ls.delete(1);
      CheckEquals(VAL_1,ls.getString(0));
      CheckEquals(1,ls.size());
      ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    ls.delete(0);
      CheckEquals(VAL_2,ls.getString(0));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_2,ls.getString(0));
        ls.delete(0);
        CheckEquals(0,ls.size());

  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
    c.Reset();
    ls.delete(2);
      CheckEquals(VAL_2,ls.getString(1));
      CheckEquals(2,ls.size());
      ls.delete(1);
        CheckEquals(1,ls.size());
        CheckEquals(VAL_1,ls.getString(0));
        ls.delete(0);
        CheckEquals(0,ls.size());
end;

procedure TSDODataObjectList_BaseTest.get_boolean_cursor();
const VAL_1 : TSDOBoolean = True; VAL_2 : TSDOBoolean = False; VAL_3 : TSDOBoolean = True;
      INT_VAL_1 = 1; INT_VAL_2 = 0; INT_VAL_3 = 1;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOBooleanType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getBoolean();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(False);
  Check(c.MoveLast());
  PByte(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := INT_VAL_1;
  CheckEquals(1,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getBoolean());
    CheckEquals(False,c.MoveNext());
    PByte(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := INT_VAL_2;
    CheckEquals(VAL_2,ls.getBoolean());
    PByte(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := INT_VAL_1;

  ls.append(False);
  Check(c.MoveLast());
  PByte(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := INT_VAL_2;
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getBoolean());
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,ls.getBoolean());
    CheckEquals(False,c.MoveNext());

  ls.append(False);
  Check(c.MoveLast());
  PByte(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := INT_VAL_3;
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,ls.getBoolean());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,ls.getBoolean());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,ls.getBoolean());
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOBooleanType.Create(FFactory));
  c := ls.getCursor();
  ls.append(False);
  ls.append(False);
  ls.append(False);
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(False,ls.getBoolean());
    CheckEquals(True,c.MoveNext());
      CheckEquals(False,ls.getBoolean());
    CheckEquals(True,c.MoveNext());
      CheckEquals(False,ls.getBoolean());
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.get_boolean_index();
const VAL_1 = True; VAL_2 = False; VAL_3 = True;
      INT_VAL_1 = 1; INT_VAL_2 = 0; INT_VAL_3 = 1;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOBooleanType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getBoolean(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(False);
  Check(c.MoveLast());
  PByte(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := INT_VAL_1;
  CheckEquals(1,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,ls.getBoolean(0));
    PByte(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := INT_VAL_2;
    CheckEquals(VAL_2,ls.getBoolean(0));
    PByte(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := INT_VAL_1;

  ls.append(False);
  Check(c.MoveLast());
  PByte(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := INT_VAL_2;
    CheckEquals(2,ls.size());
    CheckEquals(VAL_1,ls.getBoolean(0));
    CheckEquals(VAL_2,ls.getBoolean(1));

  ls.append(False);
  Check(c.MoveLast());
  PByte(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := INT_VAL_3;
      CheckEquals(3,ls.size());
      CheckEquals(VAL_1,ls.getBoolean(0));
      CheckEquals(VAL_2,ls.getBoolean(1));
      CheckEquals(VAL_3,ls.getBoolean(2));

  ls := Create_List(TSDOBooleanType.Create(FFactory));
  c := ls.getCursor();
  ls.append(False);
  ls.append(False);
  ls.append(False);
    CheckEquals(3,ls.size());
    CheckEquals(False,ls.getBoolean(0));
    CheckEquals(False,ls.getBoolean(1));
    CheckEquals(False,ls.getBoolean(2));

  ok := False;
  try
    ls.getBoolean(3);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);
end;

procedure TSDODataObjectList_BaseTest.get_byte_cursor();
const VAL_1 : TSDOByte = 12; VAL_2 : TSDOByte = 34; VAL_3 : TSDOByte = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOByteType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getByte();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(TSDOByte(0));
  Check(c.MoveLast());
  PByte(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;
  CheckEquals(1,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getByte());
    CheckEquals(False,c.MoveNext());
    PByte(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(VAL_2,ls.getByte());
    PByte(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;

  ls.append(TSDOByte(0));
  Check(c.MoveLast());
  PByte(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getByte());
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,ls.getByte());
    CheckEquals(False,c.MoveNext());

  ls.append(TSDOByte(0));
  Check(c.MoveLast());
  PByte(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_3;
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,ls.getByte());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,ls.getByte());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,ls.getByte());
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOByteType.Create(FFactory));
  c := ls.getCursor();
  ls.append(TSDOByte(0));
  ls.append(TSDOByte(0));
  ls.append(TSDOByte(0));
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,ls.getByte());
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,ls.getByte());
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,ls.getByte());
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.get_byte_index();
const VAL_1 : TSDOByte = 12; VAL_2 : TSDOByte = 34; VAL_3 : TSDOByte = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOByteType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getByte(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(TSDOByte(0));
  Check(c.MoveLast());
  PByte(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;
  CheckEquals(1,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,ls.getByte(0));
    PByte(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(VAL_2,ls.getByte(0));
    PByte(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;

  ls.append(TSDOByte(0));
  Check(c.MoveLast());
  PByte(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(2,ls.size());
    CheckEquals(VAL_1,ls.getByte(0));
    CheckEquals(VAL_2,ls.getByte(1));

  ls.append(TSDOByte(0));
  Check(c.MoveLast());
  PByte(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_3;
      CheckEquals(3,ls.size());
      CheckEquals(VAL_1,ls.getByte(0));
      CheckEquals(VAL_2,ls.getByte(1));
      CheckEquals(VAL_3,ls.getByte(2));

  ls := Create_List(TSDOByteType.Create(FFactory));
  c := ls.getCursor();
  ls.append(TSDOByte(0));
  ls.append(TSDOByte(0));
  ls.append(TSDOByte(0));
    CheckEquals(3,ls.size());
    CheckEquals(0,ls.getByte(0));
    CheckEquals(0,ls.getByte(1));
    CheckEquals(0,ls.getByte(2));

  ok := False;
  try
    ls.getByte(3);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);
end;

procedure TSDODataObjectList_BaseTest.get_dataObject_cursor();
var
  VAL_1, VAL_2, VAL_3 : ISDODataObject;
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  VAL_1 := FFactory.createNew(s_URI_1,s_TYPE_1);
  VAL_2 := FFactory.createNew(s_URI_1,s_TYPE_1);
  VAL_3 := FFactory.createNew(s_URI_1,s_TYPE_1);

  ls := Create_List(FFactory.getType(s_URI_1,s_TYPE_1));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getDataObject();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(NIL_OBJECT);
  Check(c.MoveLast());
  PPSDODataObject(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_1;
  CheckEquals(1,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    Check(VAL_1 = ls.getDataObject());
    CheckEquals(False,c.MoveNext());
    PPSDODataObject(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_2;
    Check(VAL_2 = ls.getDataObject());
    PPSDODataObject(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_1;

  ls.append(nil);
  Check(c.MoveLast());
  PPSDODataObject(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_2;
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    Check(VAL_1 = ls.getDataObject());
    CheckEquals(True,c.MoveNext());
    Check(VAL_2 = ls.getDataObject());
    CheckEquals(False,c.MoveNext());

  ls.append(nil);
  Check(c.MoveLast());
  PPSDODataObject(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_3;
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      Check(VAL_1 = ls.getDataObject());
    CheckEquals(True,c.MoveNext());
      Check(VAL_2 = ls.getDataObject());
    CheckEquals(True,c.MoveNext());
      Check(VAL_3 = ls.getDataObject());
    CheckEquals(False,c.MoveNext());

  ls := Create_List(FFactory.getType(s_URI_1,s_TYPE_1));
  c := ls.getCursor();
  ls.append(nil);
  ls.append(nil);
  ls.append(nil);
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      Check(nil = ls.getDataObject());
    CheckEquals(True,c.MoveNext());
      Check(nil = ls.getDataObject());
    CheckEquals(True,c.MoveNext());
      Check(nil = ls.getDataObject());
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.get_dataObject_index();
var
  VAL_1, VAL_2, VAL_3 : ISDODataObject;
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  VAL_1 := FFactory.createNew(s_URI_1,s_TYPE_1);
  VAL_2 := FFactory.createNew(s_URI_1,s_TYPE_1);
  VAL_3 := FFactory.createNew(s_URI_1,s_TYPE_1);

  ls := Create_List(FFactory.getType(s_URI_1,s_TYPE_1));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getDataObject(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(nil);
  Check(c.MoveLast());
  PPSDODataObject(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_1;
  CheckEquals(1,ls.size());
  CheckEquals(True,c.MoveFirst());
    Check(VAL_1 = ls.getDataObject(0));
    PPSDODataObject(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_2;
    Check(VAL_2 = ls.getDataObject(0));
    PPSDODataObject(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_1;

  ls.append(nil);
  Check(c.MoveLast());
  PPSDODataObject(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_2;
    CheckEquals(2,ls.size());
    Check(VAL_1 = ls.getDataObject(0));
    Check(VAL_2 = ls.getDataObject(1));

  ls.append(nil);
  Check(c.MoveLast());
  PPSDODataObject(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_3;
      CheckEquals(3,ls.size());
      Check(VAL_1 = ls.getDataObject(0));
      Check(VAL_2 = ls.getDataObject(1));
      Check(VAL_3 = ls.getDataObject(2));

  ls := Create_List(FFactory.getType(s_URI_1,s_TYPE_1));
  c := ls.getCursor();
  ls.append(nil);
  ls.append(nil);
  ls.append(nil);
    CheckEquals(3,ls.size());
    Check(nil = ls.getDataObject(0));
    Check(nil = ls.getDataObject(1));
    Check(nil = ls.getDataObject(2));

  ok := False;
  try
    ls.getDataObject(3);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);
end;

procedure TSDODataObjectList_BaseTest.get_date_cursor();
const VAL_1 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_2 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_3 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );

  procedure SetConstants();
  var
    d : TSDODate;
  begin
    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(1976,10,12,23,34,45,56);
    d.HourOffset := 5;
    d.MinuteOffset := 6;
    PSDODate(@VAL_1)^ := d;

    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(2008,7,8,9,10,11,12);
    d.HourOffset := 0;
    d.MinuteOffset := 13;
    PSDODate(@VAL_2)^ := d;
  end;

var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  SetConstants();
  ls := Create_List(TSDODateTimeType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getDate();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(ZERO_DATE);
  Check(c.MoveLast());
  PSDODateTime(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;
  CheckEquals(1,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getDate());
    CheckEquals(False,c.MoveNext());
    PSDODateTime(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(VAL_2,ls.getDate());
    PSDODateTime(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;

  ls.append(ZERO_DATE);
  Check(c.MoveLast());
  PSDODateTime(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getDate());
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,ls.getDate());
    CheckEquals(False,c.MoveNext());

  ls.append(ZERO_DATE);
  Check(c.MoveLast());
  PSDODateTime(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_3;
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,ls.getDate());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,ls.getDate());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,ls.getDate());
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDODateTimeType.Create(FFactory));
  c := ls.getCursor();
  ls.append(ZERO_DATE);
  ls.append(ZERO_DATE);
  ls.append(ZERO_DATE);
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(ZERO_DATE,ls.getDate());
    CheckEquals(True,c.MoveNext());
      CheckEquals(ZERO_DATE,ls.getDate());
    CheckEquals(True,c.MoveNext());
      CheckEquals(ZERO_DATE,ls.getDate());
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.get_date_index();
const VAL_1 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_2 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_3 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );

  procedure SetConstants();
  var
    d : TSDODate;
  begin
    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(1976,10,12,23,34,45,56);
    d.HourOffset := 5;
    d.MinuteOffset := 6;
    PSDODate(@VAL_1)^ := d;

    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(2008,7,8,9,10,11,12);
    d.HourOffset := 0;
    d.MinuteOffset := 13;
    PSDODate(@VAL_2)^ := d;
  end;

var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  SetConstants();
  ls := Create_List(TSDODateTimeType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getDate(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(ZERO_DATE);
  Check(c.MoveLast());
  PSDODateTime(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;
  CheckEquals(1,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,ls.getDate(0));
    PSDODateTime(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(VAL_2,ls.getDate(0));
    PSDODateTime(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;

  ls.append(ZERO_DATE);
  Check(c.MoveLast());
  PSDODateTime(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(2,ls.size());
    CheckEquals(VAL_1,ls.getDate(0));
    CheckEquals(VAL_2,ls.getDate(1));

  ls.append(ZERO_DATE);
  Check(c.MoveLast());
  PSDODateTime(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_3;
      CheckEquals(3,ls.size());
      CheckEquals(VAL_1,ls.getDate(0));
      CheckEquals(VAL_2,ls.getDate(1));
      CheckEquals(VAL_3,ls.getDate(2));

  ls := Create_List(TSDODateTimeType.Create(FFactory));
  c := ls.getCursor();
  ls.append(ZERO_DATE);
  ls.append(ZERO_DATE);
  ls.append(ZERO_DATE);
    CheckEquals(3,ls.size());
    CheckEquals(ZERO_DATE,ls.getDate(0));
    CheckEquals(ZERO_DATE,ls.getDate(1));
    CheckEquals(ZERO_DATE,ls.getDate(2));

  ok := False;
  try
    ls.getDate(3);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);
end;

procedure TSDODataObjectList_BaseTest.get_integer_cursor();
const VAL_1 : TSDOInteger = 1210; VAL_2 : TSDOInteger = -97456; VAL_3 : TSDOInteger = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOIntegerType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getInteger();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(0);
  Check(c.MoveLast());
  PSDOInteger(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;
  CheckEquals(1,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getInteger());
    CheckEquals(False,c.MoveNext());
    PSDOInteger(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(VAL_2,ls.getInteger());
    PSDOInteger(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;

  ls.append(0);
  Check(c.MoveLast());
  PSDOInteger(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getInteger());
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,ls.getInteger());
    CheckEquals(False,c.MoveNext());

  ls.append(0);
  Check(c.MoveLast());
  PSDOInteger(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_3;
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,ls.getInteger());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,ls.getInteger());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,ls.getInteger());
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOIntegerType.Create(FFactory));
  c := ls.getCursor();
  ls.append(0);
  ls.append(0);
  ls.append(0);
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,ls.getInteger());
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,ls.getInteger());
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,ls.getInteger());
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.get_integer_index();
const VAL_1 : TSDOInteger = 1210; VAL_2 : TSDOInteger = -97456; VAL_3 : TSDOInteger = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOIntegerType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getInteger(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(0);
  Check(c.MoveLast());
  PSDOInteger(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;
  CheckEquals(1,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,ls.getInteger(0));
    PSDOInteger(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(VAL_2,ls.getInteger(0));
    PSDOInteger(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_1;

  ls.append(0);
  Check(c.MoveLast());
  PSDOInteger(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_2;
    CheckEquals(2,ls.size());
    CheckEquals(VAL_1,ls.getInteger(0));
    CheckEquals(VAL_2,ls.getInteger(1));

  ls.append(0);
  Check(c.MoveLast());
  PSDOInteger(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^ := VAL_3;
      CheckEquals(3,ls.size());
      CheckEquals(VAL_1,ls.getInteger(0));
      CheckEquals(VAL_2,ls.getInteger(1));
      CheckEquals(VAL_3,ls.getInteger(2));

  ls := Create_List(TSDOIntegerType.Create(FFactory));
  c := ls.getCursor();
  ls.append(0);
  ls.append(0);
  ls.append(0);
    CheckEquals(3,ls.size());
    CheckEquals(0,ls.getInteger(0));
    CheckEquals(0,ls.getInteger(1));
    CheckEquals(0,ls.getInteger(2));

  ok := False;
  try
    ls.getInteger(3);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);
end;

procedure TSDODataObjectList_BaseTest.get_string_cursor();
const VAL_1 : TSDOString = 'xxsqklmhgf[df]121ffsd0'; VAL_2 : TSDOString = ''; VAL_3 : TSDOString = 'aeiuoou';
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOStringType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getString();
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append('');
  Check(c.MoveLast());
  PPSDOString(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_1;
  CheckEquals(1,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getString());
    CheckEquals(False,c.MoveNext());
    PPSDOString(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_2;
    CheckEquals(VAL_2,ls.getString());
    PPSDOString(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_1;

  ls.append('');
  Check(c.MoveLast());
  PPSDOString(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_2;
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,ls.getString());
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,ls.getString());
    CheckEquals(False,c.MoveNext());

  ls.append('');
  Check(c.MoveLast());
  PPSDOString(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_3;
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,ls.getString());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,ls.getString());
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,ls.getString());
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOStringType.Create(FFactory));
  c := ls.getCursor();
  ls.append('');
  ls.append('');
  ls.append('');
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals('',ls.getString());
    CheckEquals(True,c.MoveNext());
      CheckEquals('',ls.getString());
    CheckEquals(True,c.MoveNext());
      CheckEquals('',ls.getString());
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.get_string_index();
const VAL_1 = 'sdlksxhgf[df]1ffsd0'; VAL_2 = ''; VAL_3 = 'sssddaeiuoou';
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOStringType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.getString(0);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append('');
  Check(c.MoveLast());
  PPSDOString(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_1;
  CheckEquals(1,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,ls.getString(0));
    PPSDOString(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_2;
    CheckEquals(VAL_2,ls.getString(0));
    PPSDOString(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_1;

  ls.append('');
  Check(c.MoveLast());
  PPSDOString(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_2;
    CheckEquals(2,ls.size());
    CheckEquals(VAL_1,ls.getString(0));
    CheckEquals(VAL_2,ls.getString(1));

  ls.append('');
  Check(c.MoveLast());
  PPSDOString(PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^ := VAL_3;
      CheckEquals(3,ls.size());
      CheckEquals(VAL_1,ls.getString(0));
      CheckEquals(VAL_2,ls.getString(1));
      CheckEquals(VAL_3,ls.getString(2));

  ls := Create_List(TSDOStringType.Create(FFactory));
  c := ls.getCursor();
  ls.append('');
  ls.append('');
  ls.append('');
    CheckEquals(3,ls.size());
    CheckEquals('',ls.getString(0));
    CheckEquals('',ls.getString(1));
    CheckEquals('',ls.getString(2));

  ok := False;
  try
    ls.getString(3);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);
end;

procedure TSDODataObjectList_BaseTest.insert_boolean();
var
  ls : ISDODataObjectList;

  procedure CheckInvalidIndex(const AIndex : PtrInt; const AValue : TSDOBoolean);
  var
    pass : Boolean;
    oldSize : PtrInt;
  begin
    oldSize := ls.size();
    pass := False;
    try
      ls.insert(-10,AValue);
    except
      on e : ESDOIndexOutOfRangeException do begin
        pass := True;
      end;
    end;
    CheckEquals(True,pass, Format('Index = %d',[AIndex]));
    CheckEquals(oldSize, ls.size(), 'A failed "insert" must not modify the list''s size.');
  end;

var
  c : ILinkedListCursor;
  val_1, val_2, val_3 : TSDOBoolean;
begin
  val_1 := True;
  val_2 := True;
  val_3 := False;
  ls := Create_List(TSDOBooleanType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  CheckInvalidIndex(-12, val_1);
  CheckInvalidIndex(-1, val_1);
  CheckInvalidIndex(1, val_1);
  CheckInvalidIndex(12, val_1);

  ls.insert(0,val_1);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_1, ls.getBoolean(0));
    ls.insert(1,val_2);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getBoolean(0));
      CheckEquals(val_2, ls.getBoolean(1));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getBoolean(0));
        CheckEquals(val_2, ls.getBoolean(1));
        CheckEquals(val_3, ls.getBoolean(2));

  ls := Create_List(TSDOBooleanType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insert(0,val_2);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_2, ls.getBoolean(0));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getBoolean(0));
      CheckEquals(val_2, ls.getBoolean(1));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getBoolean(0));
        CheckEquals(val_2, ls.getBoolean(1));
        CheckEquals(val_3, ls.getBoolean(2));

  ls := Create_List(TSDOBooleanType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insert(0,val_3);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_3, ls.getBoolean(0));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getBoolean(0));
      CheckEquals(val_3, ls.getBoolean(1));
      ls.insert(1,val_2);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getBoolean(0));
        CheckEquals(val_2, ls.getBoolean(1));
        CheckEquals(val_3, ls.getBoolean(2));
end;

procedure TSDODataObjectList_BaseTest.insert_byte();
var
  ls : ISDODataObjectList;

  procedure CheckInvalidIndex(const AIndex : PtrInt; const AValue : TSDOByte);
  var
    pass : Boolean;
    oldSize : PtrInt;
  begin
    oldSize := ls.size();
    pass := False;
    try
      ls.insert(-10,AValue);
    except
      on e : ESDOIndexOutOfRangeException do begin
        pass := True;
      end;
    end;
    CheckEquals(True,pass, Format('Index = %d',[AIndex]));
    CheckEquals(oldSize, ls.size(), 'A failed "insert" must not modify the list''s size.');
  end;

var
  c : ILinkedListCursor;
  val_1, val_2, val_3 : TSDOByte;
begin
  val_1 := RandomRange(Low(TSDOByte),High(TSDOByte));
  val_2 := RandomRange(Low(TSDOByte),High(TSDOByte));
  val_3 := RandomRange(Low(TSDOByte),High(TSDOByte));
  ls := Create_List(TSDOByteType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  CheckInvalidIndex(-12, val_1);
  CheckInvalidIndex(-1, val_1);
  CheckInvalidIndex(1, val_1);
  CheckInvalidIndex(12, val_1);

  ls.insert(0,val_1);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_1, ls.getByte(0));
    ls.insert(1,val_2);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getByte(0));
      CheckEquals(val_2, ls.getByte(1));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getByte(0));
        CheckEquals(val_2, ls.getByte(1));
        CheckEquals(val_3, ls.getByte(2));

  ls := Create_List(TSDOByteType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insert(0,val_2);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_2, ls.getByte(0));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getByte(0));
      CheckEquals(val_2, ls.getByte(1));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getByte(0));
        CheckEquals(val_2, ls.getByte(1));
        CheckEquals(val_3, ls.getByte(2));

  ls := Create_List(TSDOByteType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insert(0,val_3);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_3, ls.getByte(0));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getByte(0));
      CheckEquals(val_3, ls.getByte(1));
      ls.insert(1,val_2);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getByte(0));
        CheckEquals(val_2, ls.getByte(1));
        CheckEquals(val_3, ls.getByte(2));
end;

procedure TSDODataObjectList_BaseTest.insert_dataObject();
var
  ls : ISDODataObjectList;

  procedure CheckInvalidIndex(const AIndex : PtrInt; const AValue : ISDODataObject);
  var
    pass : Boolean;
    oldSize : PtrInt;
  begin
    oldSize := ls.size();
    pass := False;
    try
      ls.insert(-10,AValue);
    except
      on e : ESDOIndexOutOfRangeException do begin
        pass := True;
      end;
    end;
    CheckEquals(True,pass, Format('Index = %d',[AIndex]));
    CheckEquals(oldSize, ls.size(), 'A failed "insert" must not modify the list''s size.');
  end;

var
  c : ILinkedListCursor;
  val_1, val_2, val_3 : ISDODataObject;
begin
  val_1 := FFactory.createNew(s_URI_1,s_TYPE_1);
  val_2 := FFactory.createNew(s_URI_1,s_TYPE_1);
  val_3 := FFactory.createNew(s_URI_1,s_TYPE_1);
  ls := Create_List(FFactory.getType(s_URI_1,s_TYPE_1));
  c := ls.getCursor();

  CheckInvalidIndex(-12, val_1);
  CheckInvalidIndex(-1, val_1);
  CheckInvalidIndex(1, val_1);
  CheckInvalidIndex(12, val_1);

  ls.insert(0,val_1);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(PtrUInt(val_1), PtrUInt(ls.getDataObject(0)));
    ls.insert(1,val_2);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(PtrUInt(val_1), PtrUInt(ls.getDataObject(0)));
      CheckEquals(PtrUInt(val_2), PtrUInt(ls.getDataObject(1)));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(PtrUInt(val_1), PtrUInt(ls.getDataObject(0)));
        CheckEquals(PtrUInt(val_2), PtrUInt(ls.getDataObject(1)));
        CheckEquals(PtrUInt(val_3), PtrUInt(ls.getDataObject(2)));

  ls := Create_List(FFactory.getType(s_URI_1,s_TYPE_1));
  c := ls.getCursor();
  ls.insert(0,val_2);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(PtrUInt(val_2), PtrUInt(ls.getDataObject(0)));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(PtrUInt(val_1), PtrUInt(ls.getDataObject(0)));
      CheckEquals(PtrUInt(val_2), PtrUInt(ls.getDataObject(1)));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(PtrUInt(val_1), PtrUInt(ls.getDataObject(0)));
        CheckEquals(PtrUInt(val_2), PtrUInt(ls.getDataObject(1)));
        CheckEquals(PtrUInt(val_3), PtrUInt(ls.getDataObject(2)));

  ls := Create_List(FFactory.getType(s_URI_1,s_TYPE_1));
  c := ls.getCursor();
  ls.insert(0,val_3);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(PtrUInt(val_3), PtrUInt(ls.getDataObject(0)));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(PtrUInt(val_1), PtrUInt(ls.getDataObject(0)));
      CheckEquals(PtrUInt(val_3), PtrUInt(ls.getDataObject(1)));
      ls.insert(1,val_2);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(PtrUInt(val_1), PtrUInt(ls.getDataObject(0)));
        CheckEquals(PtrUInt(val_2), PtrUInt(ls.getDataObject(1)));
        CheckEquals(PtrUInt(val_3), PtrUInt(ls.getDataObject(2)));
end;

procedure TSDODataObjectList_BaseTest.insert_date();
var
  ls : ISDODataObjectList;

  procedure CheckInvalidIndex(const AIndex : PtrInt; const AValue : TSDODateTime);
  var
    pass : Boolean;
    oldSize : PtrInt;
  begin
    oldSize := ls.size();
    pass := False;
    try
      ls.insert(-10,AValue);
    except
      on e : ESDOIndexOutOfRangeException do begin
        pass := True;
      end;
    end;
    CheckEquals(True,pass, Format('Index = %d',[AIndex]));
    CheckEquals(oldSize, ls.size(), 'A failed "insert" must not modify the list''s size.');
  end;

var
  c : ILinkedListCursor;
  val_1, val_2, val_3 : TSDODateTime;
begin
  val_1 := RandomDate();
  val_2 := RandomDate();
  val_3 := RandomDate();
  ls := Create_List(TSDODateTimeType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  CheckInvalidIndex(-12, val_1);
  CheckInvalidIndex(-1, val_1);
  CheckInvalidIndex(1, val_1);
  CheckInvalidIndex(12, val_1);

  ls.insert(0,val_1);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_1, ls.getDate(0));
    ls.insert(1,val_2);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getDate(0));
      CheckEquals(val_2, ls.getDate(1));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getDate(0));
        CheckEquals(val_2, ls.getDate(1));
        CheckEquals(val_3, ls.getDate(2));

  ls := Create_List(TSDODateTimeType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insert(0,val_2);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_2, ls.getDate(0));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getDate(0));
      CheckEquals(val_2, ls.getDate(1));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getDate(0));
        CheckEquals(val_2, ls.getDate(1));
        CheckEquals(val_3, ls.getDate(2));

  ls := Create_List(TSDODateTimeType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insert(0,val_3);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_3, ls.getDate(0));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getDate(0));
      CheckEquals(val_3, ls.getDate(1));
      ls.insert(1,val_2);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getDate(0));
        CheckEquals(val_2, ls.getDate(1));
        CheckEquals(val_3, ls.getDate(2));
end;

procedure TSDODataObjectList_BaseTest.insert_integer();
var
  ls : ISDODataObjectList;

  procedure CheckInvalidIndex(const AIndex : PtrInt; const AValue : TSDOInteger);
  var
    pass : Boolean;
    oldSize : PtrInt;
  begin
    oldSize := ls.size();
    pass := False;
    try
      ls.insert(-10,AValue);
    except
      on e : ESDOIndexOutOfRangeException do begin
        pass := True;
      end;
    end;
    CheckEquals(True,pass, Format('Index = %d',[AIndex]));
    CheckEquals(oldSize, ls.size(), 'A failed "insert" must not modify the list''s size.');
  end;

var
  c : ILinkedListCursor;
  val_1, val_2, val_3 : TSDOInteger;
begin
  val_1 := RandomRange(-12345,12345);
  val_2 := RandomRange(-12345,12345);
  val_3 := RandomRange(-12345,12345);
  ls := Create_List(TSDOIntegerType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
                            
  CheckInvalidIndex(-12, val_1);
  CheckInvalidIndex(-1, val_1);
  CheckInvalidIndex(1, val_1);
  CheckInvalidIndex(12, val_1);  

  ls.insert(0,val_1);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_1, ls.getInteger(0));
    ls.insert(1,val_2);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getInteger(0));
      CheckEquals(val_2, ls.getInteger(1));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getInteger(0));
        CheckEquals(val_2, ls.getInteger(1));
        CheckEquals(val_3, ls.getInteger(2));

  ls := Create_List(TSDOIntegerType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insert(0,val_2);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_2, ls.getInteger(0));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getInteger(0));
      CheckEquals(val_2, ls.getInteger(1));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getInteger(0));
        CheckEquals(val_2, ls.getInteger(1));
        CheckEquals(val_3, ls.getInteger(2));

  ls := Create_List(TSDOIntegerType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insert(0,val_3);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_3, ls.getInteger(0));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getInteger(0));
      CheckEquals(val_3, ls.getInteger(1));
      ls.insert(1,val_2);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getInteger(0));
        CheckEquals(val_2, ls.getInteger(1));
        CheckEquals(val_3, ls.getInteger(2));
end;

procedure TSDODataObjectList_BaseTest.insert_string();
var
  ls : ISDODataObjectList;

  procedure CheckInvalidIndex(const AIndex : PtrInt; const AValue : TSDOString);
  var
    pass : Boolean;
    oldSize : PtrInt;
  begin
    oldSize := ls.size();
    pass := False;
    try
      ls.insert(-10,AValue);
    except
      on e : ESDOIndexOutOfRangeException do begin
        pass := True;
      end;
    end;
    CheckEquals(True,pass, Format('Index = %d',[AIndex]));
    CheckEquals(oldSize, ls.size(), 'A failed "insert" must not modify the list''s size.');
  end;

var
  c : ILinkedListCursor;
  val_1, val_2, val_3 : TSDOString;
begin
  val_1 := RandomString(RandomRange(0,123));
  val_2 := RandomString(RandomRange(0,123));
  val_3 := RandomString(RandomRange(0,123));
  ls := Create_List(TSDOStringType.Create(FFactory) as ISDOType);
  c := ls.getCursor();

  CheckInvalidIndex(-12, val_1);
  CheckInvalidIndex(-1, val_1);
  CheckInvalidIndex(1, val_1);
  CheckInvalidIndex(12, val_1);

  ls.insert(0,val_1);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_1, ls.getString(0));
    ls.insert(1,val_2);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getString(0));
      CheckEquals(val_2, ls.getString(1));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getString(0));
        CheckEquals(val_2, ls.getString(1));
        CheckEquals(val_3, ls.getString(2));

  ls := Create_List(TSDOStringType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insert(0,val_2);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_2, ls.getString(0));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getString(0));
      CheckEquals(val_2, ls.getString(1));
      ls.insert(2,val_3);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(2,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getString(0));
        CheckEquals(val_2, ls.getString(1));
        CheckEquals(val_3, ls.getString(2));

  ls := Create_List(TSDOStringType.Create(FFactory) as ISDOType);
  c := ls.getCursor();
  ls.insert(0,val_3);
    CheckEquals(1, ls.size(), 'size()');
    CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
    CheckEquals(val_3, ls.getString(0));
    ls.insert(0,val_1);
      CheckEquals(2, ls.size(), 'size()');
      CheckEquals(0,c.GetPosition(),'getCursor().GetPosition()');
      CheckEquals(val_1, ls.getString(0));
      CheckEquals(val_3, ls.getString(1));
      ls.insert(1,val_2);
        CheckEquals(3, ls.size(), 'size()');
        CheckEquals(1,c.GetPosition(),'getCursor().GetPosition()');
        CheckEquals(val_1, ls.getString(0));
        CheckEquals(val_2, ls.getString(1));
        CheckEquals(val_3, ls.getString(2));
end;

procedure TSDODataObjectList_BaseTest.SetUp();
begin
  inherited;
  FFactory := Create_Factory();
end;

procedure TSDODataObjectList_BaseTest.set_boolean_cursor();
const VAL_1 = True; VAL_2 = False; VAL_3 = True;
      INT_VAL_1 = 1; INT_VAL_2 = 0; INT_VAL_3 = 1;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOBooleanType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setBoolean(VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(False);
  c.MoveFirst();
  ls.setBoolean(VAL_1);
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1, 0 <> PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
    ls.setBoolean(VAL_2);
    CheckEquals(VAL_2, 0 <> PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    ls.setBoolean(VAL_1);

  ls.append(False);
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  ls.setBoolean(VAL_2);
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1, 0 <> PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2, 0 <> PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls.append(False);
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  CheckEquals(True,c.MoveNext());
  ls.setBoolean(VAL_3);
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1, 0 <> PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2, 0 <> PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3, 0 <> PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOBooleanType.Create(FFactory));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  CheckEquals(True,c.MoveFirst());
    ls.setBoolean(False);
  CheckEquals(True,c.MoveNext());
    ls.setBoolean(False);
  CheckEquals(True,c.MoveNext());
    ls.setBoolean(False);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(False, 0 <> PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(False, 0 <> PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(False, 0 <> PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.set_boolean_index();
const VAL_1 = True; VAL_2 = False; VAL_3 = True;
      INT_VAL_1 = 1; INT_VAL_2 = 0; INT_VAL_3 = 1;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOBooleanType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setBoolean(1,VAL_1);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(False);
  ls.setBoolean(0,VAL_1);
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1, 0 <> PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
    ls.setBoolean(0,VAL_2);
    CheckEquals(VAL_2, 0 <> PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    ls.setBoolean(0,VAL_1);

  ls.append(False);
  ls.setBoolean(1,VAL_2);
  CheckEquals(2,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1, 0 <> PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2, 0 <> PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls.append(False);
  ls.setBoolean(2,VAL_3);
  CheckEquals(3,ls.size());
    CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1, 0 <> PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2, 0 <> PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3, 0 <> PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOBooleanType.Create(FFactory));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  ls.setBoolean(0,False);
  ls.setBoolean(1,False);
  ls.setBoolean(2,False);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(False, 0 <> PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(False, 0 <> PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(False, 0 <> PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.set_byte_cursor();
const VAL_1 : TSDOByte = 12; VAL_2 : TSDOByte = 34; VAL_3 : TSDOByte = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOByteType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setByte(VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(TSDOByte(0));
  c.MoveFirst();
  ls.setByte(VAL_1);
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
    ls.setByte(VAL_2);
    CheckEquals(VAL_2,PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    ls.setByte(VAL_1);

  ls.append(TSDOByte(0));
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  ls.setByte(VAL_2);
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls.append(TSDOByte(0));
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  CheckEquals(True,c.MoveNext());
  ls.setByte(VAL_3);
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOByteType.Create(FFactory));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  CheckEquals(True,c.MoveFirst());
    ls.setByte(0);
  CheckEquals(True,c.MoveNext());
    ls.setByte(0);
  CheckEquals(True,c.MoveNext());
    ls.setByte(0);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.set_byte_index();
const VAL_1 : TSDOByte = 12; VAL_2 : TSDOByte = 34; VAL_3 : TSDOByte = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOByteType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setByte(1,VAL_1);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(TSDOByte(0));
  ls.setByte(0,VAL_1);
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
    ls.setByte(0,VAL_2);
    CheckEquals(VAL_2,PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    ls.setByte(0,VAL_1);

  ls.append(TSDOByte(0));
  ls.setByte(1,VAL_2);
  CheckEquals(2,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls.append(TSDOByte(0));
  ls.setByte(2,VAL_3);
  CheckEquals(3,ls.size());
    CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOByteType.Create(FFactory));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  ls.setByte(0,0);
  ls.setByte(1,0);
  ls.setByte(2,0);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PByte( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.set_dataObject_cursor();
var
  VAL_1, VAL_2, VAL_3 : ISDODataObject;
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  VAL_1 := FFactory.createNew(s_URI_1,s_TYPE_1);
  VAL_2 := FFactory.createNew(s_URI_1,s_TYPE_1);
  VAL_3 := FFactory.createNew(s_URI_1,s_TYPE_1);

  ls := Create_List(FFactory.getType(s_URI_1,s_TYPE_1));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setDataObject(VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(nil);
  c.MoveFirst();
  ls.setDataObject(VAL_1);
  c.Reset();
  CheckEquals(True,c.MoveNext());
    Check(VAL_1 = PPSDODataObject( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());
    ls.setDataObject(VAL_2);
    Check(VAL_2 = PPSDODataObject( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    ls.setDataObject(VAL_1);

  ls.append(nil);
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  ls.setDataObject(VAL_2);
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    Check(VAL_1 = PPSDODataObject( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
    Check(VAL_2 = PPSDODataObject( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());

  ls.append(nil);
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  CheckEquals(True,c.MoveNext());
  ls.setDataObject(VAL_3);
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      Check(VAL_1 = PPSDODataObject( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      Check(VAL_2 = PPSDODataObject( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      Check(VAL_3 = PPSDODataObject( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(FFactory.getType(s_URI_1,s_TYPE_1));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  CheckEquals(True,c.MoveFirst());
    ls.setDataObject(nil);
  CheckEquals(True,c.MoveNext());
    ls.setDataObject(nil);
  CheckEquals(True,c.MoveNext());
    ls.setDataObject(nil);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      Check(nil = PPSDODataObject( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      Check(nil = PPSDODataObject( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      Check(nil = PPSDODataObject( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.set_dataObject_index();
var
  VAL_1, VAL_2, VAL_3 : ISDODataObject;
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  VAL_1 := FFactory.createNew(s_URI_1,s_TYPE_1);
  VAL_2 := FFactory.createNew(s_URI_1,s_TYPE_1);
  VAL_3 := FFactory.createNew(s_URI_1,s_TYPE_1);

  ls := Create_List(FFactory.getType(s_URI_1,s_TYPE_1));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setDataObject(1,VAL_1);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(nil);
  ls.setDataObject(0,VAL_1);
  CheckEquals(True,c.MoveFirst());
    Check(VAL_1 = PPSDODataObject( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());
    ls.setDataObject(0,VAL_2);
    Check(VAL_2 = PPSDODataObject( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    ls.setDataObject(0,VAL_1);

  ls.append(nil);
  ls.setDataObject(1,VAL_2);
  CheckEquals(2,ls.size());
  CheckEquals(True,c.MoveFirst());
    Check(VAL_1 = PPSDODataObject( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
    Check(VAL_2 = PPSDODataObject( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());

  ls.append(nil);
  ls.setDataObject(2,VAL_3);
  CheckEquals(3,ls.size());
    CheckEquals(True,c.MoveFirst());
      Check(VAL_1 = PPSDODataObject( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      Check(VAL_2 = PPSDODataObject( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      Check(VAL_3 = PPSDODataObject( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(FFactory.getType(s_URI_1,s_TYPE_1));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  ls.setDataObject(0,nil);
  ls.setDataObject(1,nil);
  ls.setDataObject(2,nil);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      Check(nil = PPSDODataObject( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      Check(nil = PPSDODataObject( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      Check(nil = PPSDODataObject( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.set_date_cursor();
const VAL_1 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_2 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_3 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );

  procedure SetConstants();
  var
    d : TSDODate;
  begin
    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(1976,10,12,23,34,45,56);
    d.HourOffset := 5;
    d.MinuteOffset := 6;
    PSDODate(@VAL_1)^ := d;

    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(2008,7,8,9,10,11,12);
    d.HourOffset := 0;
    d.MinuteOffset := 13;
    PSDODate(@VAL_2)^ := d;
  end;

var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  SetConstants();
  ls := Create_List(TSDODateTimeType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setDate(VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(ZERO_DATE);
  c.MoveFirst();
  ls.setDate(VAL_1);
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PSDODateTime( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
    ls.setDate(VAL_2);
    CheckEquals(VAL_2,PSDODateTime( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    ls.setDate(VAL_1);

  ls.append(ZERO_DATE);
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  ls.setDate(VAL_2);
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PSDODateTime( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PSDODateTime( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls.append(ZERO_DATE);
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  CheckEquals(True,c.MoveNext());
  ls.setDate(VAL_3);
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,PSDODateTime( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PSDODateTime( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PSDODateTime( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDODateTimeType.Create(FFactory));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  CheckEquals(True,c.MoveFirst());
    ls.setDate(ZERO_DATE);
  CheckEquals(True,c.MoveNext());
    ls.setDate(ZERO_DATE);
  CheckEquals(True,c.MoveNext());
    ls.setDate(ZERO_DATE);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(ZERO_DATE,PSDODateTime( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(ZERO_DATE,PSDODateTime( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(ZERO_DATE,PSDODateTime( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.set_date_index();
const VAL_1 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_2 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );
      VAL_3 : TSDODate = ( Date : 0; HourOffset : 0; MinuteOffset : 0; );

  procedure SetConstants();
  var
    d : TSDODate;
  begin
    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(1976,10,12,23,34,45,56);
    d.HourOffset := 5;
    d.MinuteOffset := 6;
    PSDODate(@VAL_1)^ := d;

    FillChar(d,SizeOf(TSDODate),#0);
    d.Date := EncodeDateTime(2008,7,8,9,10,11,12);
    d.HourOffset := 0;
    d.MinuteOffset := 13;
    PSDODate(@VAL_2)^ := d;
  end;

var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  SetConstants();
  ls := Create_List(TSDODateTimeType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setDate(1,VAL_1);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(ZERO_DATE);
  ls.setDate(0,VAL_1);
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PSDODateTime( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
    ls.setDate(0,VAL_2);
    CheckEquals(VAL_2,PSDODateTime( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    ls.setDate(0,VAL_1);

  ls.append(ZERO_DATE);
  ls.setDate(1,VAL_2);
  CheckEquals(2,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PSDODateTime( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PSDODateTime( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls.append(ZERO_DATE);
  ls.setDate(2,VAL_3);
  CheckEquals(3,ls.size());
    CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,PSDODateTime( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PSDODateTime( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PSDODateTime( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDODateTimeType.Create(FFactory));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  ls.setDate(0,ZERO_DATE);
  ls.setDate(1,ZERO_DATE);
  ls.setDate(2,ZERO_DATE);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(ZERO_DATE,PSDODateTime( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(ZERO_DATE,PSDODateTime( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(ZERO_DATE,PSDODateTime( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.set_integer_cursor();
const VAL_1 : TSDOInteger = 1210; VAL_2 : TSDOInteger = -97456; VAL_3 : TSDOInteger = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOIntegerType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setInteger(VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(0);
  c.MoveFirst();
  ls.setInteger(VAL_1);
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PSDOInteger( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
    ls.setInteger(VAL_2);
    CheckEquals(VAL_2,PSDOInteger( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    ls.setInteger(VAL_1);

  ls.append(0);
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  ls.setInteger(VAL_2);
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PSDOInteger( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PSDOInteger( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls.append(0);
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  CheckEquals(True,c.MoveNext());
  ls.setInteger(VAL_3);
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,PSDOInteger( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PSDOInteger( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PSDOInteger( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOIntegerType.Create(FFactory));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  CheckEquals(True,c.MoveFirst());
    ls.setInteger(0);
  CheckEquals(True,c.MoveNext());
    ls.setInteger(0);
  CheckEquals(True,c.MoveNext());
    ls.setInteger(0);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOInteger( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOInteger( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOInteger( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.set_integer_index();
const VAL_1 : TSDOInteger = 1210; VAL_2 : TSDOInteger = -97456; VAL_3 : TSDOInteger = 0;
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOIntegerType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setInteger(1,VAL_1);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append(0);
  ls.setInteger(0,VAL_1);
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PSDOInteger( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
    ls.setInteger(0,VAL_2);
    CheckEquals(VAL_2,PSDOInteger( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    ls.setInteger(0,VAL_1);

  ls.append(0);
  ls.setInteger(1,VAL_2);
  CheckEquals(2,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PSDOInteger( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PSDOInteger( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls.append(0);
  ls.setInteger(2,VAL_3);
  CheckEquals(3,ls.size());
    CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,PSDOInteger( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PSDOInteger( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PSDOInteger( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOIntegerType.Create(FFactory));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  ls.setInteger(0,0);
  ls.setInteger(1,0);
  ls.setInteger(2,0);

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOInteger( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOInteger( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(0,PSDOInteger( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.set_string_cursor();
const VAL_1 = 'aqs##[df]}}'; VAL_2 = ''; VAL_3 = '?^* mlqdbcnd,';
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOStringType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setString(VAL_1);
  except
    on e : ESDOIllegalArgumentException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append('');
  c.MoveFirst();
  ls.setString(VAL_1);
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PPSDOString( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());
    ls.setString(VAL_2);
    CheckEquals(VAL_2,PPSDOString( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    ls.setString(VAL_1);

  ls.append('');
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  ls.setString(VAL_2);
  CheckEquals(2,ls.size());
  c.Reset();
  CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_1,PPSDOString( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PPSDOString( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());

  ls.append('');
  CheckEquals(True,c.MoveFirst());
  CheckEquals(True,c.MoveNext());
  CheckEquals(True,c.MoveNext());
  ls.setString(VAL_3);
  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_1,PPSDOString( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PPSDOString( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PPSDOString( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOStringType.Create(FFactory));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  CheckEquals(True,c.MoveFirst());
    ls.setString('');
  CheckEquals(True,c.MoveNext());
    ls.setString('');
  CheckEquals(True,c.MoveNext());
    ls.setString('');

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals('',PPSDOString( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      CheckEquals('',PPSDOString( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      CheckEquals('',PPSDOString( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.set_string_index();
const VAL_1 = '""##aqs##[df]}}'; VAL_2 = ''; VAL_3 = '##?^* mldhbcnd,';
var
  ls : ISDODataObjectList;
  c : ILinkedListCursor;
  ok : Boolean;
begin
  ls := Create_List(TSDOStringType.Create(FFactory));
  c := ls.getCursor();

  CheckEquals(0,ls.size());
  Check(nil <> c);
  ok := False;
  try
    ls.setString(1,VAL_1);
  except
    on e : ESDOIndexOutOfRangeException do begin
      ok := True;
    end;
  end;
  CheckEquals(True,ok);

  ls.append('');
  ls.setString(0,VAL_1);
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PPSDOString( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());
    ls.setString(0,VAL_2);
    CheckEquals(VAL_2,PPSDOString( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    ls.setString(0,VAL_1);

  ls.append('');
  ls.setString(1,VAL_2);
  CheckEquals(2,ls.size());
  CheckEquals(True,c.MoveFirst());
    CheckEquals(VAL_1,PPSDOString( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
    CheckEquals(VAL_2,PPSDOString( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());

  ls.append('');
  ls.setString(2,VAL_3);
  CheckEquals(3,ls.size());
    CheckEquals(True,c.MoveFirst());
      CheckEquals(VAL_1,PPSDOString( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_2,PPSDOString( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      CheckEquals(VAL_3,PPSDOString( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());

  ls := Create_List(TSDOStringType.Create(FFactory));
  c := ls.getCursor();
  ls.append(VAL_1);
  ls.append(VAL_2);
  ls.append(VAL_3);
  ls.setString(0,'');
  ls.setString(1,'');
  ls.setString(2,'');

  CheckEquals(3,ls.size());
  c.Reset();
    CheckEquals(True,c.MoveNext());
      CheckEquals('',PPSDOString( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      CheckEquals('',PPSDOString( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(True,c.MoveNext());
      CheckEquals('',PPSDOString( PtrUInt(GetData(c.GetCurrent())) + VALUE_STATUS_LENGTH )^^);
    CheckEquals(False,c.MoveNext());
end;

procedure TSDODataObjectList_BaseTest.TearDown();
begin
  FFactory := nil;
  inherited;
end;

{ TSDODataObjectList_Test }

class function TSDODataObjectList_Test.Create_List(AItemType: ISDOType): ISDODataObjectList;
begin
  Result := TSDODataObjectList.Create(AItemType);
end;

initialization
  RegisterTest('object',TSDODataObjectList_Test.Suite);

end.
