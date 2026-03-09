program demo_dynamic;
{$mode delphi}{$H+}
{$RTTI EXPLICIT FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])}

uses
  SysUtils, TypInfo, Rtti,
  System.JSON.Types,
  System.JSON.Writers, System.JSON.Readers,
  System.JSON.Serializers, System.JSON.Converters;

type
  { Types we cannot modify — simulate third-party or generated code }
  TApiResponse = record
    StatusCode: Integer;
    Message: string;
    InternalId: string;
    DebugTrace: string;
    Payload: string;
  end;

  TUserProfile = record
    FirstName: string;
    LastName: string;
    Email: string;
    PasswordHash: string;
    Age: Integer;
    Bio: string;
  end;

  TColor = (Red, Green, Blue, Yellow);

  TSetting = record
    Key: string;
    Value: string;
    Priority: TColor;
  end;

procedure Section(const Title: string);
begin
  WriteLn;
  WriteLn('=== ', Title, ' ===');
end;

procedure DemoFieldRenaming;
var
  Resolver: TJsonDynamicContractResolver;
  Ser: TJsonSerializer;
  R: TApiResponse;
begin
  Section('Runtime Field Renaming');

  WriteLn('  Scenario: API returns snake_case, our record uses PascalCase.');
  WriteLn('  Use SetFieldName to map between the two at runtime.');
  WriteLn;

  Resolver := TJsonDynamicContractResolver.Create;
  Ser := TJsonSerializer.Create;
  try
    Resolver.SetFieldName(TypeInfo(TApiResponse), 'StatusCode', 'status_code');
    Resolver.SetFieldName(TypeInfo(TApiResponse), 'Message', 'message');
    Resolver.SetFieldName(TypeInfo(TApiResponse), 'InternalId', 'internal_id');
    Resolver.SetFieldName(TypeInfo(TApiResponse), 'DebugTrace', 'debug_trace');
    Resolver.SetFieldName(TypeInfo(TApiResponse), 'Payload', 'payload');

    Ser.ContractResolver := Resolver;
    // Resolver is now owned by the serializer via interface ref-counting

    R.StatusCode := 200;
    R.Message := 'OK';
    R.InternalId := 'req-42';
    R.DebugTrace := 'n/a';
    R.Payload := 'Hello World';

    WriteLn('  Serialized with snake_case field names:');
    WriteLn('    ', Ser.Serialize<TApiResponse>(R));

    WriteLn;
    WriteLn('  Deserialized from snake_case JSON:');
    R := Ser.Deserialize<TApiResponse>(
      '{"status_code":404,"message":"Not Found","internal_id":"req-99","debug_trace":"","payload":""}');
    WriteLn('    StatusCode=', R.StatusCode, ', Message=', R.Message,
            ', InternalId=', R.InternalId);
  finally
    Ser.Free;
  end;
end;

procedure DemoFieldIgnoring;
var
  Resolver: TJsonDynamicContractResolver;
  Ser, SerDefault: TJsonSerializer;
  U: TUserProfile;
begin
  Section('Runtime Field Ignoring');

  WriteLn('  Scenario: Exclude sensitive fields without modifying the type.');
  WriteLn;

  U.FirstName := 'Alice';
  U.LastName := 'Smith';
  U.Email := 'alice@example.com';
  U.PasswordHash := '$2b$12$secret_hash_here';
  U.Age := 30;
  U.Bio := 'Software engineer';

  SerDefault := TJsonSerializer.Create;
  try
    WriteLn('  Default serialization (all fields):');
    WriteLn('    ', SerDefault.Serialize<TUserProfile>(U));
  finally
    SerDefault.Free;
  end;

  WriteLn;

  Resolver := TJsonDynamicContractResolver.Create;
  Ser := TJsonSerializer.Create;
  try
    Resolver.SetFieldsIgnored(TypeInfo(TUserProfile), ['PasswordHash']);
    Resolver.SetFieldName(TypeInfo(TUserProfile), 'FirstName', 'first_name');
    Resolver.SetFieldName(TypeInfo(TUserProfile), 'LastName', 'last_name');

    Ser.ContractResolver := Resolver;

    WriteLn('  With PasswordHash ignored + field renaming:');
    WriteLn('    ', Ser.Serialize<TUserProfile>(U));
    WriteLn;
    WriteLn('  PasswordHash is completely omitted from output.');
  finally
    Ser.Free;
  end;
end;

procedure DemoSelectiveInclusion;
var
  Resolver: TJsonDynamicContractResolver;
  Ser: TJsonSerializer;
  R: TApiResponse;
begin
  Section('Selective Field Inclusion via Ignore');

  WriteLn('  Scenario: Only include specific fields by ignoring the rest.');
  WriteLn;

  Resolver := TJsonDynamicContractResolver.Create;
  Ser := TJsonSerializer.Create;
  try
    // Ignore the internal/debug fields, keep only the public-facing ones
    Resolver.SetFieldsIgnored(TypeInfo(TApiResponse), ['InternalId', 'DebugTrace']);

    Ser.ContractResolver := Resolver;

    R.StatusCode := 200;
    R.Message := 'Success';
    R.InternalId := 'req-123';
    R.DebugTrace := 'very long trace...';
    R.Payload := '{"data":42}';

    WriteLn('  With InternalId and DebugTrace ignored:');
    WriteLn('    ', Ser.Serialize<TApiResponse>(R));
    WriteLn;
    WriteLn('  Only StatusCode, Message, and Payload remain.');
  finally
    Ser.Free;
  end;
end;

procedure DemoFieldConverter;
var
  Resolver: TJsonDynamicContractResolver;
  Ser: TJsonSerializer;
  S: TSetting;
  Json: string;
begin
  Section('Runtime Field Converter');

  WriteLn('  Scenario: Attach an enum name converter to a specific field at runtime.');
  WriteLn;

  Resolver := TJsonDynamicContractResolver.Create;
  Ser := TJsonSerializer.Create;
  try
    Resolver.SetFieldConverter(TypeInfo(TSetting), 'Priority', TJsonEnumNameConverter);

    Ser.ContractResolver := Resolver;

    S.Key := 'theme';
    S.Value := 'dark';
    S.Priority := Green;

    Json := Ser.Serialize<TSetting>(S);
    WriteLn('  With enum converter on Priority field:');
    WriteLn('    ', Json);

    S := Ser.Deserialize<TSetting>(Json);
    WriteLn;
    WriteLn('  Deserialized: Key=', S.Key, ', Value=', S.Value,
            ', Priority=', GetEnumName(TypeInfo(TColor), Ord(S.Priority)));
  finally
    Ser.Free;
  end;
end;

procedure DemoClearAndReset;
var
  Resolver: TJsonDynamicContractResolver;
  Ser: TJsonSerializer;
  U: TUserProfile;
begin
  Section('ClearAttributes — Reset to Defaults');

  Resolver := TJsonDynamicContractResolver.Create;
  Ser := TJsonSerializer.Create;
  try
    Ser.ContractResolver := Resolver;

    U.FirstName := 'Bob';
    U.LastName := 'Jones';
    U.Email := 'bob@test.com';
    U.PasswordHash := 'hash123';
    U.Age := 25;
    U.Bio := 'Tester';

    // Apply customizations
    Resolver.SetFieldsIgnored(TypeInfo(TUserProfile), ['PasswordHash', 'Bio']);
    Resolver.SetFieldName(TypeInfo(TUserProfile), 'FirstName', 'fname');

    WriteLn('  With dynamic customizations (ignore + rename):');
    WriteLn('    ', Ser.Serialize<TUserProfile>(U));

    // Clear all dynamic attributes
    Resolver.ClearAttributes;

    WriteLn;
    WriteLn('  After ClearAttributes (back to defaults):');
    WriteLn('    ', Ser.Serialize<TUserProfile>(U));
    WriteLn;
    WriteLn('  All fields restored, original names used.');
  finally
    Ser.Free;
  end;
end;

begin
  WriteLn('Demo: Dynamic Contract Resolver — Runtime Customization');
  WriteLn('========================================================');

  DemoFieldRenaming;
  DemoFieldIgnoring;
  DemoSelectiveInclusion;
  DemoFieldConverter;
  DemoClearAndReset;

  WriteLn;
  WriteLn('Done.');
end.
