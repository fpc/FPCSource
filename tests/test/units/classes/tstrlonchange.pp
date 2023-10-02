program sl;
//issue #40261
{$mode objfpc}
{$WARN 5024 off : Parameter "$1" not used}
uses
  SysUtils, Classes;

type
  TTest = class
  private
    FList: TStringList;
    FCount: Integer;
    procedure Change(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure DoTest;
  end;

procedure assertOK(B : Boolean; Msg : String);

begin
  if not B then
    begin
    Writeln(Msg);
    Halt(1);
    end;
end;

procedure TTest.Change(Sender: TObject);
begin
  Inc(FCount);
end;

constructor TTest.Create;
begin
  FCount := 0;
  FList := TStringList.Create;
  FList.OwnsObjects := True;
  FList.OnChange := @Change;
end;

destructor TTest.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

procedure TTest.DoTest;
begin
  FCount := 0;
  FList.AddObject('Added',TObject.Create);
  AssertOK(FCount=1,format('AddObject causes %d OnChanges, expected 1',[FCount]));

  FCount := 0;
  FList.InsertObject(0, 'Inserted',TObject.Create);
  AssertOK(FCount=1,format('InsertObject causes %d OnChanges, expected 1',[FCount]));

  FCount := 0;
  FList.AddPair('Name','Value',TObject.Create);
  AssertOK(FCount=1,format('InsertObject causes %d OnChanges, expected 1',[FCount]));
end;

var
  T: TTest;

begin
  T := TTest.Create;
  try
    T.DoTest;
  finally
    T.Free;
  end;
end.

