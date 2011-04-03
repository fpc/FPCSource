{ a helper hides an existing enumerator }
program tchlp53;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TContainerEnum = class;

  TContainer = class
    Contents: array[0..5] of Integer;
    function GetEnumerator: TContainerEnum;
    constructor Create;
  end;

  TContainerEnum = class
  private
    fIndex: Integer;
    fContainer: TContainer;
    fForward: Boolean;
  public
    constructor Create(aContainer: TContainer; aForward: Boolean);
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
    Contents[i] := i;
end;

function TContainer.GetEnumerator: TContainerEnum;
begin
  Result := TContainerEnum.Create(Self, True);
end;

{ TContainerHelper }

function TContainerHelper.GetEnumerator: TContainerEnum;
begin
  Result := TContainerEnum.Create(Self, False);
end;

{ TContainerEnum }

constructor TContainerEnum.Create(aContainer: TContainer; aForward: Boolean);
begin
  fContainer := aContainer;
  fForward := aForward;
  if fForward then
    fIndex := Low(fContainer.Contents) - 1
  else
    fIndex := High(fContainer.Contents) + 1;
end;

function TContainerEnum.GetCurrent: Integer;
begin
  Result := fContainer.Contents[fIndex];
end;

function TContainerEnum.MoveNext: Boolean;
begin
  if fForward then begin
    Inc(fIndex);
    Result := fIndex <= High(fContainer.Contents);
  end else begin
    Dec(fIndex);
    Result := fIndex >= Low(fContainer.Contents);
  end;
end;

var
  cont: TContainer;
  i, c: Integer;
begin
  cont := TContainer.Create;
  c := 5;
  for i in cont do begin
    if c <> i then
      Halt(1);
    Writeln(i);
    Dec(c);
  end;
  Writeln('ok');
end.
