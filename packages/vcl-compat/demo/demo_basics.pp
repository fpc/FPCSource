program demo_basics;
{$mode delphi}{$H+}
{$RTTI EXPLICIT FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])}

uses
  SysUtils, TypInfo, Rtti,
  System.JSON.Serializers;

type
  TPoint = record
    X: Integer;
    Y: Integer;
  end;

  TPerson = record
    Name: string;
    Age: Integer;
    Active: Boolean;
    Score: Double;
  end;

  TAddress = record
    Street: string;
    City: string;
  end;

  TContact = record
    Name: string;
    Email: string;
    Address: TAddress;
  end;

var
  Ser: TJsonSerializer;

  procedure Section(const Title: string);
  begin
    WriteLn;
    WriteLn('=== ', Title, ' ===');
  end;

  procedure Demo(const ALabel, Json: string);
  begin
    WriteLn('  ', ALabel, ': ', Json);
  end;

  { Primitives }
  procedure DemoPrimitives;
  var
    I: Integer;
    I64: Int64;
    D: Double;
    S: string;
    B: Boolean;
    C: Char;
  begin
    Section('Primitive Types');

    // Serialize
    Demo('Integer(42)',       Ser.Serialize<Integer>(42));
    Demo('Integer(-1)',       Ser.Serialize<Integer>(-1));
    Demo('Int64(MaxInt64)',   Ser.Serialize<Int64>(High(Int64)));
    Demo('Double(3.14159)',   Ser.Serialize<Double>(3.14159));
    Demo('String(hello)',     Ser.Serialize<string>('hello'));
    Demo('String(with "q")',  Ser.Serialize<string>('She said "hi"'));
    Demo('Boolean(True)',     Ser.Serialize<Boolean>(True));
    Demo('Boolean(False)',    Ser.Serialize<Boolean>(False));
    Demo('Char(A)',           Ser.Serialize<Char>('A'));

    WriteLn;
    WriteLn('  Round-trip verification:');

    // Round-trip
    I := Ser.Deserialize<Integer>(Ser.Serialize<Integer>(42));
    WriteLn('    Integer: 42 -> serialize -> deserialize -> ', I);

    I64 := Ser.Deserialize<Int64>(Ser.Serialize<Int64>(9876543210));
    WriteLn('    Int64: 9876543210 -> serialize -> deserialize -> ', I64);

    D := Ser.Deserialize<Double>(Ser.Serialize<Double>(2.71828));
    WriteLn('    Double: 2.71828 -> serialize -> deserialize -> ', D:0:5);

    S := Ser.Deserialize<string>(Ser.Serialize<string>('world'));
    WriteLn('    String: world -> serialize -> deserialize -> ', S);

    B := Ser.Deserialize<Boolean>(Ser.Serialize<Boolean>(True));
    WriteLn('    Boolean: True -> serialize -> deserialize -> ', B);

    C := Ser.Deserialize<Char>(Ser.Serialize<Char>('Z'));
    WriteLn('    Char: Z -> serialize -> deserialize -> ', C);
  end;

  { Simple records }
  procedure DemoSimpleRecords;
  var
    P, P2: TPoint;
    Pers, Pers2: TPerson;
    Json: string;
  begin
    Section('Simple Records');

    P.X := 10; P.Y := 20;
    Json := Ser.Serialize<TPoint>(P);
    Demo('TPoint(10,20)', Json);

    P2 := Ser.Deserialize<TPoint>(Json);
    WriteLn('    Deserialized back: X=', P2.X, ', Y=', P2.Y);
    WriteLn;

    Pers.Name := 'Alice';
    Pers.Age := 30;
    Pers.Active := True;
    Pers.Score := 95.5;
    Json := Ser.Serialize<TPerson>(Pers);
    Demo('TPerson', Json);

    Pers2 := Ser.Deserialize<TPerson>(Json);
    WriteLn('    Deserialized: Name=', Pers2.Name, ', Age=', Pers2.Age,
            ', Active=', Pers2.Active, ', Score=', Pers2.Score:0:1);
  end;

  { Nested records }
  procedure DemoNestedRecords;
  var
    C, C2: TContact;
    Json: string;
  begin
    Section('Nested Records');

    C.Name := 'Bob';
    C.Email := 'bob@example.com';
    C.Address.Street := '123 Main St';
    C.Address.City := 'Springfield';
    Json := Ser.Serialize<TContact>(C);
    Demo('TContact', Json);

    C2 := Ser.Deserialize<TContact>(Json);
    WriteLn('    Deserialized: Name=', C2.Name, ', Email=', C2.Email);
    WriteLn('                  Street=', C2.Address.Street, ', City=', C2.Address.City);
  end;

  { Arrays }
  procedure DemoArrays;
  type
    TIntArray = array of Integer;
    TStrArray = array of string;
    TPointArray = array of TPoint;
  var
    Ints: TIntArray;
    Strs: TStrArray;
    Pts: TPointArray;
    Json: string;
    I: Integer;
  begin
    Section('Dynamic Arrays');

    Ints := TIntArray.Create(1, 2, 3, 4, 5);
    Json := Ser.Serialize<TIntArray>(Ints);
    Demo('Integer array', Json);

    Ints := Ser.Deserialize<TIntArray>(Json);
    Write('    Deserialized: [');
    for I := 0 to High(Ints) do begin
      if I > 0 then Write(', ');
      Write(Ints[I]);
    end;
    WriteLn(']');
    WriteLn;

    Strs := TStrArray.Create('hello', 'world', 'foo');
    Demo('String array', Ser.Serialize<TStrArray>(Strs));
    WriteLn;

    SetLength(Pts, 3);
    Pts[0].X := 0;  Pts[0].Y := 0;
    Pts[1].X := 10; Pts[1].Y := 5;
    Pts[2].X := 20; Pts[2].Y := 10;
    Json := Ser.Serialize<TPointArray>(Pts);
    Demo('Record array', Json);

    Pts := Ser.Deserialize<TPointArray>(Json);
    WriteLn('    Deserialized ', Length(Pts), ' points:');
    for I := 0 to High(Pts) do
      WriteLn('      [', I, '] X=', Pts[I].X, ', Y=', Pts[I].Y);

    WriteLn;
    Ints := nil;
    Demo('Empty array', Ser.Serialize<TIntArray>(Ints));
  end;

begin
  Ser := TJsonSerializer.Create;
  try
    WriteLn('Demo: Basic Serialization & Deserialization');
    WriteLn('============================================');

    DemoPrimitives;
    DemoSimpleRecords;
    DemoNestedRecords;
    DemoArrays;

    WriteLn;
    WriteLn('Done.');
  finally
    Ser.Free;
  end;
end.
