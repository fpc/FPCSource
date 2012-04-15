unit tw20872a;

{$MODE delphi}

interface

type
  TWrapper<TValue> = class end;

  TTestClass = class
  strict private
    FWrapper: TWrapper<Integer>;  { Using inline specialization }
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
