program project1;

{$mode objfpc}

type
  TSomeEvent = procedure(Sender: TObject; X, Y, Line: integer; mark: Integer) of object;

type
  TSubObject = class(TObject)
  public
    SomeEvent: TSomeEvent;
  end;

  TMyObject = class(TObject)
  private
    fSub : TSubObject;
  protected
    procedure DoSomeEvent(Sender: TObject; X, Y, Line: integer; mark: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function GetSomeEvent: TSomeEvent;
  end;

constructor TMyObject.Create;
begin
  fSub := TSubObject.Create;
  fSub.SomeEvent := @Self.DoSomeEvent;
end;

destructor TMyObject.Destroy;
begin
  fSub.Free;
end;

function TMyObject.GetSomeEvent: TSomeEvent;
begin
  Result := fSub.SomeEvent;
end;

procedure TMyObject.DoSomeEvent(Sender: TObject; X, Y, Line: integer; mark: Integer);
begin
  writeln('do some event');
end;

var
  my : TMyObject;
  mtd : TMethod;
type
  TGetProc = function (): TMethod of object;

begin
  my := TMyObject.Create;

  mtd := TGetProc(@my.GetSomeEvent)();

  writeln('mtd.Data = ', PtrInt(mtd.Data));
  writeln('mtd.Code = ', PtrInt(mtd.Code));

  if Assigned(TSomeEvent(mtd)) then
   TSomeEvent(mtd)(nil,0,0,0,0);

  my.Free;
end.
