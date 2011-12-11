unit tw20872b;

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
begin
  FWrapper := TWrapper<Integer>.Create;  { Duplicate identifier error here }
end;

destructor TTestClass.Destroy;
begin
  FWrapper.Free;
end;

end.
