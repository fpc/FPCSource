{ a class helper may introduce a enumerator }
program tchlp42;

{$ifdef fpc}
  {$mode objfpc}
{$endif}
{$apptype console}

type
  TContainer = class
    Contents: array[0..5] of Integer;
    constructor Create;
  end;

  TContainerEnum = class
  private
    fIndex: Integer;
    fContainer: TContainer;
  public
    constructor Create(aContainer: TContainer);
    function GetCurrent: Integer;
    function MoveNext: Boolean;
    property Current: Integer read GetCurrent;
  end;

  TContainerHelper = class helper for TContainer
    function GetEnumerator: TContainerEnum;
  end;

{ TContainer }

constructor TContainer.Create;
var
  i: Integer;
begin
  for i := Low(Contents) to High(Contents) do
    Contents[i] := High(Contents) - i;
end;

{ TContainerHelper }

function TContainerHelper.GetEnumerator: TContainerEnum;
begin
  Result := TContainerEnum.Create(Self);
end;

{ TContainerEnum }

constructor TContainerEnum.Create(aContainer: TContainer);
begin
  fContainer := aContainer;
  fIndex := Low(fContainer.Contents) - 1;
end;

function TContainerEnum.GetCurrent: Integer;
begin
  Result := fContainer.Contents[fIndex];
end;

function TContainerEnum.MoveNext: Boolean;
begin
  Inc(fIndex);
  Result := fIndex <= High(fContainer.Contents);
end;

var
  cont: TContainer;
  i: Integer;
begin
  cont := TContainer.Create;
  for i in cont do
    Writeln(i);
  Writeln('ok');
end.
