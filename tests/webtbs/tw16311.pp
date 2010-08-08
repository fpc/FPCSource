{ %opt=-gh }

{$APPTYPE CONSOLE}

{$ifdef fpc}
  {$mode delphi}
{$endif}

uses SysUtils;

var
  CreatedCount, DestroyedCount: Integer;

type

  { TCntObject }

  TCntObject = class(TObject)
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TMyObject }

  TMyObject = class(TCntObject)
  private
    FSubObject: TCntObject;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;

{ TCntObject }

constructor TCntObject.Create;
begin
  Inc(CreatedCount);
end;

destructor TCntObject.Destroy;
begin
  Inc(DestroyedCount);
  inherited Destroy;
end;

{ TMyObject }

constructor TMyObject.Create;
begin
  inherited Create;
  FSubObject := TCntObject.Create;
end;

destructor TMyObject.Destroy;
begin
  FSubObject.Free;
  inherited Destroy;
end;

procedure TMyObject.AfterConstruction;
begin
  raise Exception.Create('OnAfterConstruction');
end;

var
  A: TMyObject;
  gotexception: boolean;
begin
  HaltOnNotReleased := true;
  CreatedCount := 0;
  DestroyedCount := 0;
  try
    A := nil;
    try
      A := TMyObject.Create;
    finally
      A.Free;
    end;
  except
    writeln('created objects = ', CreatedCount);
    writeln('destroyed objects = ', DestroyedCount);
    gotexception:=true;
    writeln;
  end;
  if not gotexception then halt(1);
end.
