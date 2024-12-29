{ %NORUN }
{ %OPT=-Sew }

program tgeneric121;

{$mode objfpc}

type
  generic TTest<T> = class
    constructor Create; virtual;
  end;

  {$push}
  {$warn 3018 off}

  generic TTestSub<T> = class(specialize TTest<T>)
  protected
    constructor Create; override;
  end;

  {$pop}

  generic TTestSub2<T> = class(specialize TTestSub<T>)

  end;

constructor TTest.Create;
begin
end;

constructor TTestSub.Create;
begin
end;

begin

end.
