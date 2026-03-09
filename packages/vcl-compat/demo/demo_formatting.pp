program demo_formatting;
{$mode delphi}{$H+}
{$RTTI EXPLICIT FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])}

uses
  SysUtils, Classes, TypInfo, Rtti, StreamEx,
  System.JSON.Types,
  System.JSON.Writers,
  System.JSON.Serializers;

type
  TAddress = record
    Street: string;
    City: string;
    Zip: string;
  end;

  TPerson = record
    Name: string;
    Age: Integer;
    Active: Boolean;
    Address: TAddress;
  end;

  TItems = array of string;

  TMessage = record
    Content: string;
  end;

procedure Section(const Title: string);
begin
  WriteLn;
  WriteLn('=== ', Title, ' ===');
end;

procedure DemoCompactVsIndented;
var
  Ser: TJsonSerializer;
  P: TPerson;
begin
  Section('Compact vs Indented Output');

  P.Name := 'Alice';
  P.Age := 30;
  P.Active := True;
  P.Address.Street := '123 Main St';
  P.Address.City := 'Springfield';
  P.Address.Zip := '62701';

  Ser := TJsonSerializer.Create;
  try
    Ser.Formatting := TJsonFormatting.None;
    WriteLn('  TJsonFormatting.None (compact):');
    WriteLn('    ', Ser.Serialize<TPerson>(P));
  finally
    Ser.Free;
  end;

  WriteLn;

  Ser := TJsonSerializer.Create;
  try
    Ser.Formatting := TJsonFormatting.Indented;
    WriteLn('  TJsonFormatting.Indented (pretty-printed):');
    WriteLn(Ser.Serialize<TPerson>(P));
  finally
    Ser.Free;
  end;
end;

procedure DemoEscapeHandling;
var
  Ser: TJsonSerializer;
  M: TMessage;
begin
  Section('String Escape Handling');

  M.Content := 'Price: 5€ — <b>Bold</b> & "quoted" — Café';

  Ser := TJsonSerializer.Create;
  try
    Ser.StringEscapeHandling := TJsonStringEscapeHandling.Default;
    WriteLn('  StringEscapeHandling.Default:');
    WriteLn('    ', Ser.Serialize<TMessage>(M));
  finally
    Ser.Free;
  end;

  WriteLn;

  Ser := TJsonSerializer.Create;
  try
    Ser.StringEscapeHandling := TJsonStringEscapeHandling.EscapeNonAscii;
    WriteLn('  StringEscapeHandling.EscapeNonAscii:');
    WriteLn('    ', Ser.Serialize<TMessage>(M));
  finally
    Ser.Free;
  end;

  WriteLn;

  Ser := TJsonSerializer.Create;
  try
    Ser.StringEscapeHandling := TJsonStringEscapeHandling.EscapeHtml;
    WriteLn('  StringEscapeHandling.EscapeHtml:');
    WriteLn('    ', Ser.Serialize<TMessage>(M));
  finally
    Ser.Free;
  end;
end;

procedure DemoDirectWriterControl;
var
  SW: TStringWriter;
  JW: TJsonTextWriter;
begin
  Section('Direct TJsonTextWriter Control');

  WriteLn('  Using TJsonTextWriter directly for fine-grained output:');
  WriteLn;

  SW := TStringWriter.Create;
  JW := TJsonTextWriter.Create(SW);
  try
    JW.Formatting := TJsonFormatting.Indented;

    JW.WriteStartObject;
      JW.WritePropertyName('name');
      JW.WriteValue('Alice');
      JW.WritePropertyName('scores');
      JW.WriteStartArray;
        JW.WriteValue(95);
        JW.WriteValue(87);
        JW.WriteValue(92);
      JW.WriteEndArray;
      JW.WritePropertyName('metadata');
      JW.WriteStartObject;
        JW.WritePropertyName('created');
        JW.WriteValue('2024-01-15');
        JW.WritePropertyName('version');
        JW.WriteValue(2);
      JW.WriteEndObject;
      JW.WritePropertyName('notes');
      JW.WriteNull;
    JW.WriteEndObject;

    WriteLn(SW.ToString);
  finally
    JW.Free;
    SW.Free;
  end;
end;

procedure DemoIndentedArrays;
var
  Ser: TJsonSerializer;
  Items: TItems;
begin
  Section('Indented Arrays');

  Items := TItems.Create('alpha', 'beta', 'gamma', 'delta');

  Ser := TJsonSerializer.Create;
  try
    Ser.Formatting := TJsonFormatting.None;
    WriteLn('  Compact: ', Ser.Serialize<TItems>(Items));
  finally
    Ser.Free;
  end;

  WriteLn;

  Ser := TJsonSerializer.Create;
  try
    Ser.Formatting := TJsonFormatting.Indented;
    WriteLn('  Indented:');
    WriteLn(Ser.Serialize<TItems>(Items));
  finally
    Ser.Free;
  end;
end;

procedure DemoSerializerSettings;
var
  Ser: TJsonSerializer;
begin
  Section('Serializer Settings Overview');

  Ser := TJsonSerializer.Create;
  try
    WriteLn('  Default settings after TJsonSerializer.Create:');
    WriteLn('    Formatting:            ', GetEnumName(TypeInfo(TJsonFormatting), Ord(Ser.Formatting)));
    WriteLn('    StringEscapeHandling:  ', GetEnumName(TypeInfo(TJsonStringEscapeHandling), Ord(Ser.StringEscapeHandling)));
    WriteLn('    DateFormatHandling:    ', GetEnumName(TypeInfo(TJsonDateFormatHandling), Ord(Ser.DateFormatHandling)));
    WriteLn('    DateTimeZoneHandling:  ', GetEnumName(TypeInfo(TJsonDateTimeZoneHandling), Ord(Ser.DateTimeZoneHandling)));
    WriteLn('    FloatFormatHandling:   ', GetEnumName(TypeInfo(TJsonFloatFormatHandling), Ord(Ser.FloatFormatHandling)));
    WriteLn('    MemberSerialization:   ', GetEnumName(TypeInfo(TJsonMemberSerialization), Ord(Ser.MemberSerialization)));
    WriteLn('    ValueSerialization:    ', GetEnumName(TypeInfo(TJsonValueSerialization), Ord(Ser.ValueSerialization)));
    WriteLn('    MaxDepth:              ', Ser.MaxDepth);
  finally
    Ser.Free;
  end;
end;

begin
  WriteLn('Demo: Output Formatting & Settings');
  WriteLn('===================================');

  DemoCompactVsIndented;
  DemoEscapeHandling;
  DemoDirectWriterControl;
  DemoIndentedArrays;
  DemoSerializerSettings;

  WriteLn;
  WriteLn('Done.');
end.
