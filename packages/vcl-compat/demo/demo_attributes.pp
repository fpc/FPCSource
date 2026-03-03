program demo_attributes;
{$mode delphi}{$H+}
{$RTTI EXPLICIT FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])}

uses
  SysUtils, TypInfo, Rtti,
  System.JSON.Types,
  System.JSON.Serializers;

type
  { Plain record — no attributes, all fields serialized by default }
  TPlainUser = record
    FirstName: string;
    LastName: string;
    Password: string;
    Age: Integer;
  end;

  { Renamed fields via JsonName }
  TRenamedUser = record
    [JsonName('first_name')]
    FirstName: string;
    [JsonName('last_name')]
    LastName: string;
    Password: string;
    Age: Integer;
  end;

  { Ignored fields via JsonIgnore }
  TFilteredUser = record
    [JsonName('first_name')]
    FirstName: string;
    [JsonName('last_name')]
    LastName: string;
    [JsonIgnore]
    Password: string;
    Age: Integer;
  end;

  { Control member serialization mode at the type level }
  [JsonSerialize(TJsonMemberSerialization.&Public)]
  TPublicOnly = class
  private
    FInternal: Integer;
  public
    Name: string;
  published
    property DisplayName: string read Name write Name;
  end;

  { Force private field inclusion with JsonIn }
  TForceInclude = record
    Name: string;
    [JsonIn]
    Secret: string;    // Would be excluded in 'Public' mode, but [JsonIn] forces it
    Age: Integer;
  end;

  { Value serialization — skip default values }
  TConfig = record
    Host: string;
    Port: Integer;
    Debug: Boolean;
    Timeout: Double;
  end;

var
  Ser: TJsonSerializer;

  procedure Section(const Title: string);
  begin
    WriteLn;
    WriteLn('=== ', Title, ' ===');
  end;

  procedure DemoRenaming;
  var
    Plain: TPlainUser;
    Renamed: TRenamedUser;
    Filtered: TFilteredUser;
  begin
    Section('Field Renaming with [JsonName]');

    Plain.FirstName := 'Alice';
    Plain.LastName := 'Smith';
    Plain.Password := 's3cret';
    Plain.Age := 30;

    WriteLn('  Without attributes:');
    WriteLn('    ', Ser.Serialize<TPlainUser>(Plain));

    Renamed.FirstName := 'Alice';
    Renamed.LastName := 'Smith';
    Renamed.Password := 's3cret';
    Renamed.Age := 30;

    WriteLn;
    WriteLn('  With [JsonName(''first_name'')] and [JsonName(''last_name'')]:');
    WriteLn('    ', Ser.Serialize<TRenamedUser>(Renamed));

    WriteLn;
    WriteLn('  Deserialization uses the renamed keys:');
    Renamed := Ser.Deserialize<TRenamedUser>('{"first_name":"Bob","last_name":"Jones","Password":"pw","Age":25}');
    WriteLn('    FirstName=', Renamed.FirstName, ', LastName=', Renamed.LastName);
  end;

  procedure DemoIgnore;
  var
    U: TFilteredUser;
  begin
    Section('Excluding Fields with [JsonIgnore]');

    U.FirstName := 'Alice';
    U.LastName := 'Smith';
    U.Password := 's3cret';
    U.Age := 30;

    WriteLn('  [JsonIgnore] on Password + [JsonName] on names:');
    WriteLn('    ', Ser.Serialize<TFilteredUser>(U));
    WriteLn;
    WriteLn('  Notice: Password field is omitted from the output.');
    WriteLn('  On deserialization, ignored fields keep their default value:');
    U := Ser.Deserialize<TFilteredUser>('{"first_name":"Charlie","last_name":"Brown","Password":"leaked","Age":8}');
    WriteLn('    FirstName=', U.FirstName, ', Password="', U.Password, '" (empty - ignored)');
  end;

  procedure DemoValueSerialization;
  var
    Cfg: TConfig;
    SerVS: TJsonSerializer;
  begin
    Section('Value Serialization — Skip Defaults');

    Cfg.Host := 'localhost';
    Cfg.Port := 0;        // default for integer
    Cfg.Debug := False;    // default for boolean
    Cfg.Timeout := 0.0;    // default for float

    WriteLn('  Config with mostly default values:');
    WriteLn;
    WriteLn('  ValueSerialization = Always (default):');
    WriteLn('    ', Ser.Serialize<TConfig>(Cfg));

    SerVS := TJsonSerializer.Create;
    try
      SerVS.ValueSerialization := TJsonValueSerialization.ExcludeDefault;
      WriteLn;
      WriteLn('  ValueSerialization = ExcludeDefault:');
      WriteLn('    ', SerVS.Serialize<TConfig>(Cfg));
      WriteLn;
      WriteLn('  Only non-default values (Host="localhost") are included.');
    finally
      SerVS.Free;
    end;
  end;

  procedure DemoCombined;
  var
    U: TFilteredUser;
    Json: string;
  begin
    Section('Combined: Rename + Ignore + Round-Trip');

    U.FirstName := 'Diana';
    U.LastName := 'Prince';
    U.Password := 'wonderwoman';
    U.Age := 28;

    Json := Ser.Serialize<TFilteredUser>(U);
    WriteLn('  Serialized:   ', Json);

    U := Ser.Deserialize<TFilteredUser>(Json);
    WriteLn('  Deserialized: FirstName=', U.FirstName,
            ', LastName=', U.LastName,
            ', Age=', U.Age);
    WriteLn('                Password="', U.Password, '" (ignored, stays empty)');
  end;

begin
  Ser := TJsonSerializer.Create;
  try
    WriteLn('Demo: Attribute-Driven Serialization Control');
    WriteLn('=============================================');

    DemoRenaming;
    DemoIgnore;
    DemoValueSerialization;
    DemoCombined;

    WriteLn;
    WriteLn('Done.');
  finally
    Ser.Free;
  end;
end.
