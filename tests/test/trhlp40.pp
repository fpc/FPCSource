{ %NORUN }

{ a helper may introduce an enumerator }
program trhlp40;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TContainer = record
    Contents: array[0..5] of Integer;
    procedure Init;
  end;
  PContainer = ^TContainer;

  TContainerEnum = class
  private
    fIndex: Integer;
    fContainer: PContainer;
  public
    constructor Create(aContainer: PContainer);
    function GetCurrent: Integer;
    function MoveNext: Boolean;
    property Current: Integer read GetCurrent;
  end;

  TContainerHelper = record helper for TContainer
    function GetEnumerator: TContainerEnum;
  end;

{ TContainer }

procedure TContainer.Init;
var
  i: Integer;
begin
  for i := Low(Contents) to High(Contents) do
    Contents[i] := i;
end;

{ TContainerHelper }

function TContainerHelper.GetEnumerator: TContainerEnum;
begin
  Result := TContainerEnum.Create(@Self);
end;

{ TContainerEnum }

constructor TContainerEnum.Create(aContainer: PContainer);
begin
  fContainer := aContainer;
  fIndex := Low(fContainer^.Contents) - 1;
end;

function TContainerEnum.GetCurrent: Integer;
begin
  Result := fContainer^.Contents[fIndex];
end;

function TContainerEnum.MoveNext: Boolean;
begin
  Inc(fIndex);
  Result := fIndex <= High(fContainer^.Contents);
end;

var
  cont: TContainer;
  i: Integer;
begin
  cont.Init;
  for i in cont do ;
end.
