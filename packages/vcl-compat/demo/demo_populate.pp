program demo_populate;
{$mode delphi}{$H+}
{$RTTI EXPLICIT FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])}

uses
  SysUtils, Classes, TypInfo, Rtti, StreamEx,
  System.JSON.Types,
  System.JSON.Readers,
  System.JSON.Serializers;

type
  TAppConfig = record
    Host: string;
    Port: Integer;
    Debug: Boolean;
    LogLevel: string;
    MaxRetries: Integer;
    Timeout: Double;
  end;

  TPlayerState = record
    Name: string;
    Score: Integer;
    Level: Integer;
    Health: Integer;
    Mana: Integer;
  end;

  TCoordinate = record
    X: Double;
    Y: Double;
    Z: Double;
  end;

var
  Ser: TJsonSerializer;

  procedure Section(const Title: string);
  begin
    WriteLn;
    WriteLn('=== ', Title, ' ===');
  end;

  procedure PrintConfig(const C: TAppConfig);
  begin
    WriteLn('    Host=', C.Host, ', Port=', C.Port, ', Debug=', C.Debug);
    WriteLn('    LogLevel=', C.LogLevel, ', MaxRetries=', C.MaxRetries, ', Timeout=', C.Timeout:0:1);
  end;

  procedure PrintPlayer(const P: TPlayerState);
  begin
    WriteLn('    Name=', P.Name, ', Score=', P.Score, ', Level=', P.Level,
            ', Health=', P.Health, ', Mana=', P.Mana);
  end;

  procedure DemoConfigOverlay;
  var
    Cfg: TAppConfig;
  begin
    Section('Config Defaults + User Overrides');

    WriteLn('  Step 1: Set defaults in code');
    Cfg.Host := 'localhost';
    Cfg.Port := 8080;
    Cfg.Debug := False;
    Cfg.LogLevel := 'info';
    Cfg.MaxRetries := 3;
    Cfg.Timeout := 30.0;
    PrintConfig(Cfg);

    WriteLn;
    WriteLn('  Step 2: Populate from user config (only overrides Port and Debug):');
    Ser.Populate<TAppConfig>('{"Port":9090,"Debug":true}', Cfg);
    PrintConfig(Cfg);
    WriteLn;
    WriteLn('  Notice: Host, LogLevel, MaxRetries, Timeout unchanged.');

    WriteLn;
    WriteLn('  Step 3: Populate from environment config (override Host and LogLevel):');
    Ser.Populate<TAppConfig>('{"Host":"production.example.com","LogLevel":"warn"}', Cfg);
    PrintConfig(Cfg);
    WriteLn;
    WriteLn('  Final config is the merge of defaults + user + environment.');
  end;

  procedure DemoPartialUpdate;
  var
    Player: TPlayerState;
  begin
    Section('Partial Updates (Game State)');

    WriteLn('  Initial player state:');
    Player.Name := 'Hero';
    Player.Score := 1000;
    Player.Level := 5;
    Player.Health := 100;
    Player.Mana := 50;
    PrintPlayer(Player);

    WriteLn;
    WriteLn('  After combat (update Score and Health only):');
    Ser.Populate<TPlayerState>('{"Score":1250,"Health":72}', Player);
    PrintPlayer(Player);

    WriteLn;
    WriteLn('  After level up (update Level and Mana):');
    Ser.Populate<TPlayerState>('{"Level":6,"Mana":65}', Player);
    PrintPlayer(Player);

    WriteLn;
    WriteLn('  After healing (restore Health):');
    Ser.Populate<TPlayerState>('{"Health":100}', Player);
    PrintPlayer(Player);
  end;

  procedure DemoPopulateFromReader;
  var
    Coord: TCoordinate;
    SR: TStringReader;
    JR: TJsonTextReader;
  begin
    Section('Populate from TJsonReader');

    Coord.X := 1.0;
    Coord.Y := 2.0;
    Coord.Z := 3.0;

    WriteLn('  Original: X=', Coord.X:0:1, ', Y=', Coord.Y:0:1, ', Z=', Coord.Z:0:1);

    SR := TStringReader.Create('{"X":10.5,"Z":99.9}');
    JR := TJsonTextReader.Create(SR);
    try
      WriteLn;
      WriteLn('  Populate from reader with {"X":10.5,"Z":99.9}:');
      Ser.Populate<TCoordinate>(JR, Coord);
      WriteLn('    X=', Coord.X:0:1, ', Y=', Coord.Y:0:1, ', Z=', Coord.Z:0:1);
      WriteLn('    Y unchanged, X and Z updated.');
    finally
      JR.Free; // CloseInput frees SR
    end;
  end;

  procedure DemoSerializeThenPopulate;
  var
    Cfg1, Cfg2: TAppConfig;
    Json: string;
  begin
    Section('Serialize + Populate Round-Trip');

    WriteLn('  Source config:');
    Cfg1.Host := 'api.example.com';
    Cfg1.Port := 443;
    Cfg1.Debug := False;
    Cfg1.LogLevel := 'error';
    Cfg1.MaxRetries := 5;
    Cfg1.Timeout := 60.0;
    PrintConfig(Cfg1);

    Json := Ser.Serialize<TAppConfig>(Cfg1);
    WriteLn;
    WriteLn('  Serialized: ', Json);

    WriteLn;
    WriteLn('  Populate a fresh config from that JSON:');
    Cfg2 := Default(TAppConfig);
    Ser.Populate<TAppConfig>(Json, Cfg2);
    PrintConfig(Cfg2);

    WriteLn;
    WriteLn('  Compare with Deserialize<T> (creates a new value):');
    Cfg2 := Ser.Deserialize<TAppConfig>(Json);
    PrintConfig(Cfg2);
  end;

begin
  Ser := TJsonSerializer.Create;
  try
    WriteLn('Demo: Populate — Update Existing Instances from JSON');
    WriteLn('=====================================================');

    DemoConfigOverlay;
    DemoPartialUpdate;
    DemoPopulateFromReader;
    DemoSerializeThenPopulate;

    WriteLn;
    WriteLn('Done.');
  finally
    Ser.Free;
  end;
end.
