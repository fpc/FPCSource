{ %NORUN }
{ %OPT=-Sew }

program tgeneric120;

{$mode objfpc}

type
  generic TTest<T> = class
    constructor Create; virtual;
  end;

  {$warn 3018 off}

  generic TTestSub<T> = class(specialize TTest<T>)
  protected
    constructor Create; override;
  end;

constructor TTest.Create;
begin
end;

constructor TTestSub.Create;
begin
end;

begin

end.
