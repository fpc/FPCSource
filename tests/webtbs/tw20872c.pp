unit tw20872c;

{$MODE delphi}

interface

type
  TWrapper<TValue> = class end;

  TTestClass = class
  strict private
    type TSpecializedWrapper = TWrapper<Integer>;
  strict private
    FWrapper: TSpecializedWrapper;  { Using traditional workaround }
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TTestClass.Create;
type
  TLocalSpecializedWrapper =
    TWrapper<Integer>;  { Duplicate identifier error here }
begin
  FWrapper := TLocalSpecializedWrapper.Create;
end;

destructor TTestClass.Destroy;
begin
  FWrapper.Free;
end;

end.
