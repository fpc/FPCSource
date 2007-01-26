{$ifdef fpc}
  {$mode delphi}
{$endif}

{$i-}

uses
  SysUtils;

type
  TMyObject1 = class(TObject)
    constructor Create; virtual;
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

  TMyObject2 = class(TMyObject1)
    constructor Create; override;
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

  TMyObject3 = class(TMyObject2)
    constructor Create; override;
    destructor Destroy; override;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;


var
  Depth: Integer;
  s: string;

{ TMyObject1 }

procedure TMyObject1.AfterConstruction;
begin
  s:=s+'1a';
  WriteLn(StringOfChar(' ', Depth * 2), '-> TMyObject1.AfterConstruction'); Inc(Depth); try
  inherited;
  s:=s+'2a';
  finally 
    Dec(Depth); WriteLn(StringOfChar(' ', Depth * 2), '<- TMyObject1.AfterConstruction');
    s:=s+'3a';
  end;
  s:=s+'4a';
end;

procedure TMyObject1.BeforeDestruction;
begin
  s:=s+'1b';
  WriteLn(StringOfChar(' ', Depth * 2), '-> TMyObject1.BeforeDestruction'); Inc(Depth); try
  inherited;
  s:=s+'2b';
  finally
    Dec(Depth); WriteLn(StringOfChar(' ', Depth * 2), '<- TMyObject1.BeforeDestruction');
    s:=s+'3b';
  end;
  s:=s+'4b';
end;

constructor TMyObject1.Create;
begin
  s:=s+'1c';
  WriteLn(StringOfChar(' ', Depth * 2), '-> TMyObject1.Create'); Inc(Depth); try
  inherited;
  s:=s+'2c';
  raise Exception.Create('');
  s:=s+'3c';
  finally
    Dec(Depth); WriteLn(StringOfChar(' ', Depth * 2), '<- TMyObject1.Create');
    s:=s+'4c';
  end;
  s:=s+'5c';
end;

destructor TMyObject1.Destroy;
begin
  s:=s+'1d';
  WriteLn(StringOfChar(' ', Depth * 2), '-> TMyObject1.Destroy'); Inc(Depth); try
  inherited;
  s:=s+'2d';
  finally
    Dec(Depth); WriteLn(StringOfChar(' ', Depth * 2), '<- TMyObject1.Destroy');
    s:=s+'3d';
  end;
  s:=s+'4d';
end;

procedure TMyObject1.FreeInstance;
begin
  s:=s+'1e';
  WriteLn(StringOfChar(' ', Depth * 2), '-> TMyObject1.FreeInstance'); Inc(Depth); try
  inherited;
  s:=s+'2e';
  finally
    Dec(Depth); WriteLn(StringOfChar(' ', Depth * 2), '<- TMyObject1.FreeInstance');
    s:=s+'3e';
  end;
  s:=s+'4e';
end;

class function TMyObject1.NewInstance: TObject;
begin
  s:=s+'1f';
  WriteLn(StringOfChar(' ', Depth * 2), '-> TMyObject1.NewInstance'); Inc(Depth); try
  Result := inherited NewInstance;
  s:=s+'2f';
  finally
    Dec(Depth); WriteLn(StringOfChar(' ', Depth * 2), '<- TMyObject1.NewInstance');
    s:=s+'3f';
  end;
  s:=s+'4f';
end;

{ TMyObject2 }

procedure TMyObject2.AfterConstruction;
begin
  s:=s+'1g';
  WriteLn(StringOfChar(' ', Depth * 2), '-> TMyObject2.AfterConstruction'); Inc(Depth); try
  inherited;
  s:=s+'2g';
  finally
     Dec(Depth); WriteLn(StringOfChar(' ', Depth * 2), '<- TMyObject2.AfterConstruction'); 
    s:=s+'3g';
  end;
  s:=s+'4g';
end;

procedure TMyObject2.BeforeDestruction;
begin
  s:=s+'1h';
  WriteLn(StringOfChar(' ', Depth * 2), '-> TMyObject2.BeforeDestruction'); Inc(Depth); try
  inherited;
  s:=s+'2h';
  finally
     Dec(Depth); WriteLn(StringOfChar(' ', Depth * 2), '<- TMyObject2.BeforeDestruction'); 
    s:=s+'3h';
  end;
  s:=s+'4h';
end;

constructor TMyObject2.Create;
begin
  s:=s+'1i';
  WriteLn(StringOfChar(' ', Depth * 2), '-> TMyObject2.Create'); Inc(Depth); try
  inherited;
  s:=s+'2i';
  finally
     Dec(Depth); WriteLn(StringOfChar(' ', Depth * 2), '<- TMyObject2.Create'); 
    s:=s+'3i';
  end;
  s:=s+'4i';
end;

destructor TMyObject2.Destroy;
begin
  s:=s+'1j';
  WriteLn(StringOfChar(' ', Depth * 2), '-> TMyObject2.Destroy'); Inc(Depth); try
  inherited;
  s:=s+'2j';
  finally
     Dec(Depth); WriteLn(StringOfChar(' ', Depth * 2), '<- TMyObject2.Destroy'); 
    s:=s+'3j';
  end;
  s:=s+'4j';
end;

procedure TMyObject2.FreeInstance;
begin
  s:=s+'1k';
  WriteLn(StringOfChar(' ', Depth * 2), '-> TMyObject2.FreeInstance'); Inc(Depth); try
  inherited;
  s:=s+'2k';
  finally
     Dec(Depth); WriteLn(StringOfChar(' ', Depth * 2), '<- TMyObject2.FreeInstance'); 
    s:=s+'3k';
  end;
  s:=s+'4k';
end;

class function TMyObject2.NewInstance: TObject;
begin
  s:=s+'1l';
  WriteLn(StringOfChar(' ', Depth * 2), '-> TMyObject2.NewInstance'); Inc(Depth); try
  Result := inherited NewInstance;
  s:=s+'2l';
  finally
     Dec(Depth); WriteLn(StringOfChar(' ', Depth * 2), '<- TMyObject2.NewInstance'); 
    s:=s+'3l';
  end;
  s:=s+'4l';
end;

{ TMyObject3 }

procedure TMyObject3.AfterConstruction;
begin
  s:=s+'1m';
  WriteLn(StringOfChar(' ', Depth * 2), '-> TMyObject3.AfterConstruction'); Inc(Depth); try
  inherited;
  s:=s+'2m';
  finally
     Dec(Depth); WriteLn(StringOfChar(' ', Depth * 2), '<- TMyObject3.AfterConstruction'); 
    s:=s+'3m';
  end;
  s:=s+'4m';
end;

procedure TMyObject3.BeforeDestruction;
begin
  s:=s+'1n';
  WriteLn(StringOfChar(' ', Depth * 2), '-> TMyObject3.BeforeDestruction'); Inc(Depth); try
  inherited;
  s:=s+'2n';
  finally
     Dec(Depth); WriteLn(StringOfChar(' ', Depth * 2), '<- TMyObject3.BeforeDestruction'); 
    s:=s+'3n';
  end;
  s:=s+'4n';
end;

constructor TMyObject3.Create;
begin
  s:=s+'1o';
  WriteLn(StringOfChar(' ', Depth * 2), '-> TMyObject3.Create'); Inc(Depth); try
  inherited;
  s:=s+'2o';
  finally
     Dec(Depth); WriteLn(StringOfChar(' ', Depth * 2), '<- TMyObject3.Create'); 
    s:=s+'3o';
  end;
  s:=s+'4o';
end;

destructor TMyObject3.Destroy;
begin
  s:=s+'1p';
  WriteLn(StringOfChar(' ', Depth * 2), '-> TMyObject3.Destroy'); Inc(Depth); try
  inherited;
  s:=s+'2p';
  finally
     Dec(Depth); WriteLn(StringOfChar(' ', Depth * 2), '<- TMyObject3.Destroy'); 
    s:=s+'3p';
  end;
  s:=s+'4p';
end;

procedure TMyObject3.FreeInstance;
begin
  s:=s+'1q';
  WriteLn(StringOfChar(' ', Depth * 2), '-> TMyObject3.FreeInstance'); Inc(Depth); try
  inherited;
  s:=s+'2q';
  finally
     Dec(Depth); WriteLn(StringOfChar(' ', Depth * 2), '<- TMyObject3.FreeInstance'); 
    s:=s+'3q';
  end;
  s:=s+'4q';
end;

class function TMyObject3.NewInstance: TObject;
begin
  s:=s+'1r';
  WriteLn(StringOfChar(' ', Depth * 2), '-> TMyObject3.NewInstance'); Inc(Depth); try
  Result := inherited NewInstance;
  s:=s+'2r';
  finally
     Dec(Depth); WriteLn(StringOfChar(' ', Depth * 2), '<- TMyObject3.NewInstance'); 
    s:=s+'3r';
  end;
  s:=s+'4r';
end;

begin
  try
    with TMyObject3.Create do try
      WriteLn('******');
      halt(1);
    finally
      halt(1);
      Free;
    end;
  finally
    writeln(s);
    if (s <> '1r1l1f2f3f4f2l3l4l2r3r4r1o1i1c2c4c3i3o1p1j1d2d3d4d2j3j4j2p3p4p1q1k1e2e3e4e2k3k4k2q3q4q') then
      halt(1);
    halt(0);
  end;
end.

