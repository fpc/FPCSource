program demo_converters;
{$mode delphi}{$H+}
{$RTTI EXPLICIT FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])}

uses
  SysUtils, TypInfo, Rtti,
  System.JSON.Types,
  System.JSON.Writers, System.JSON.Readers,
  System.JSON.Serializers, System.JSON.Converters;

type
  TColor = (Red, Green, Blue, Yellow, Cyan, Magenta);
  TColorSet = set of TColor;

  { Record using enums and sets }
  TTheme = record
    Name: string;
    Primary: TColor;
    Accent: TColor;
    Flags: TColorSet;
  end;

  { Record with GUID }
  TSession = record
    Id: TGUID;
    UserName: string;
    Active: Boolean;
  end;

  { Custom converter: serialize TPoint2D as "X,Y" string }
  TPoint2D = record
    X: Integer;
    Y: Integer;
  end;

  TPoint2DConverter = class(TJsonConverter)
  public
    function CanConvert(ATypeInf: PTypeInfo): Boolean; override;
    procedure WriteJson(const AWriter: TJsonWriter; const AValue: TValue;
      const ASerializer: TJsonSerializer); override;
    function ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo;
      const AExistingValue: TValue; const ASerializer: TJsonSerializer): TValue; override;
  end;

  { Record for custom converter demo (converter attached via Converters list) }
  TShape = record
    Name: string;
    Origin: TPoint2D;
    Width: Integer;
    Height: Integer;
  end;

{ TPoint2DConverter }

function TPoint2DConverter.CanConvert(ATypeInf: PTypeInfo): Boolean;
begin
  Result := ATypeInf = TypeInfo(TPoint2D);
end;

procedure TPoint2DConverter.WriteJson(const AWriter: TJsonWriter; const AValue: TValue;
  const ASerializer: TJsonSerializer);
var
  P: TPoint2D;
begin
  P := AValue.AsType<TPoint2D>;
  AWriter.WriteValue(IntToStr(P.X) + ',' + IntToStr(P.Y));
end;

function TPoint2DConverter.ReadJson(const AReader: TJsonReader; ATypeInf: PTypeInfo;
  const AExistingValue: TValue; const ASerializer: TJsonSerializer): TValue;
var
  S: string;
  P: TPoint2D;
  CommaPos: Integer;
begin
  S := AReader.Value.AsString;
  CommaPos := Pos(',', S);
  P.X := StrToInt(Trim(Copy(S, 1, CommaPos - 1)));
  P.Y := StrToInt(Trim(Copy(S, CommaPos + 1, MaxInt)));
  Result := TValue.From<TPoint2D>(P);
end;

var
  Ser: TJsonSerializer;

  procedure Section(const Title: string);
  begin
    WriteLn;
    WriteLn('=== ', Title, ' ===');
  end;

  procedure DemoEnumConverter;
  var
    T: TTheme;
    Json: string;
  begin
    Section('TJsonEnumNameConverter — Enums as Strings');

    T.Name := 'Ocean';
    T.Primary := Blue;
    T.Accent := Cyan;
    T.Flags := [Blue, Cyan];

    WriteLn('  Without converter (default — enums as names):');
    WriteLn('    ', Ser.Serialize<TTheme>(T));

    WriteLn;
    WriteLn('  With TJsonEnumNameConverter added to Converters list:');
    Ser.Converters.Add(TJsonEnumNameConverter.Create);
    Json := Ser.Serialize<TTheme>(T);
    WriteLn('    ', Json);

    WriteLn;
    WriteLn('  Round-trip:');
    T := Ser.Deserialize<TTheme>(Json);
    WriteLn('    Name=', T.Name, ', Primary=', GetEnumName(TypeInfo(TColor), Ord(T.Primary)),
            ', Accent=', GetEnumName(TypeInfo(TColor), Ord(T.Accent)));
  end;

  procedure DemoSetConverter;
  var
    SerSet: TJsonSerializer;
    T: TTheme;
    Json: string;
  begin
    Section('TJsonSetNamesConverter — Sets as Comma-Separated Names');

    SerSet := TJsonSerializer.Create;
    try
      SerSet.Converters.Add(TJsonEnumNameConverter.Create);
      SerSet.Converters.Add(TJsonSetNamesConverter.Create);

      T.Name := 'Sunset';
      T.Primary := Red;
      T.Accent := Yellow;
      T.Flags := [Red, Yellow, Magenta];

      Json := SerSet.Serialize<TTheme>(T);
      WriteLn('  Serialized with enum + set converters:');
      WriteLn('    ', Json);

      WriteLn;
      WriteLn('  Round-trip:');
      T := SerSet.Deserialize<TTheme>(Json);
      WriteLn('    Name=', T.Name);
      WriteLn('    Flags contains Red?    ', Red in T.Flags);
      WriteLn('    Flags contains Yellow? ', Yellow in T.Flags);
      WriteLn('    Flags contains Blue?   ', Blue in T.Flags);
    finally
      SerSet.Free;
    end;
  end;

  procedure DemoGUIDConverter;
  var
    SerG: TJsonSerializer;
    S: TSession;
    Json: string;
  begin
    Section('TJsonGUIDConverter — GUIDs as Strings');

    SerG := TJsonSerializer.Create;
    try
      SerG.Converters.Add(TJsonGUIDConverter.Create);

      S.Id := StringToGUID('{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}');
      S.UserName := 'alice';
      S.Active := True;

      Json := SerG.Serialize<TSession>(S);
      WriteLn('  Serialized TSession:');
      WriteLn('    ', Json);

      WriteLn;
      S := SerG.Deserialize<TSession>(Json);
      WriteLn('  Deserialized:');
      WriteLn('    Id=', GUIDToString(S.Id));
      WriteLn('    UserName=', S.UserName, ', Active=', S.Active);
    finally
      SerG.Free;
    end;
  end;

  procedure DemoCustomConverter;
  var
    SerC, SerDefault: TJsonSerializer;
    S: TShape;
    Json: string;
  begin
    Section('Custom TPoint2DConverter — Compact "X,Y" Format');

    WriteLn('  Without converter (default nested object):');
    SerDefault := TJsonSerializer.Create;
    try
      S.Name := 'Rectangle';
      S.Origin.X := 100;
      S.Origin.Y := 200;
      S.Width := 50;
      S.Height := 30;
      WriteLn('    ', SerDefault.Serialize<TShape>(S));
    finally
      SerDefault.Free;
    end;

    WriteLn;
    WriteLn('  With TPoint2DConverter added to Converters list:');
    SerC := TJsonSerializer.Create;
    try
      SerC.Converters.Add(TPoint2DConverter.Create);

      Json := SerC.Serialize<TShape>(S);
      WriteLn('    ', Json);
      WriteLn('  Notice: Origin is "100,200" instead of {"X":100,"Y":200}');

      WriteLn;
      S := SerC.Deserialize<TShape>(Json);
      WriteLn('  Deserialized:');
      WriteLn('    Name=', S.Name, ', Origin=(', S.Origin.X, ',', S.Origin.Y, ')');
      WriteLn('    Width=', S.Width, ', Height=', S.Height);
    finally
      SerC.Free;
    end;
  end;

  procedure DemoConverterPrecedence;
  var
    SerP: TJsonSerializer;
    S: TShape;
  begin
    Section('Converter Precedence');

    SerP := TJsonSerializer.Create;
    try
      WriteLn('  Converters are checked in this priority order:');
      WriteLn('    1. [JsonConverter] attribute on the member/field');
      WriteLn('    2. [JsonConverter] attribute on the type');
      WriteLn('    3. Converters list on the serializer (first match wins)');
      WriteLn;

      SerP.Converters.Add(TPoint2DConverter.Create);

      S.Name := 'Circle';
      S.Origin.X := 50;
      S.Origin.Y := 75;
      S.Width := 100;
      S.Height := 100;

      WriteLn('  TPoint2DConverter in Converters list:');
      WriteLn('    ', SerP.Serialize<TShape>(S));
    finally
      SerP.Free;
    end;
  end;

begin
  Ser := TJsonSerializer.Create;
  try
    WriteLn('Demo: Converters');
    WriteLn('=================');

    DemoEnumConverter;
    DemoSetConverter;
    DemoGUIDConverter;
    DemoCustomConverter;
    DemoConverterPrecedence;

    WriteLn;
    WriteLn('Done.');
  finally
    Ser.Free;
  end;
end.
