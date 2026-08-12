{ %NORUN }

program tgeneric131;

{$mode objfpc}

type
{$interfaces corba}
  IRawBase = interface
  end;

  IRawSub = interface(IRawBase)
  ['Foobar']
  end;

{$interfaces com}
  IComBase = interface
  end;

  IComSub = interface(IComBase)
  ['{48E52CE0-899F-4EB0-802B-0346BE6A547D}']
  end;

  generic TRawTest<Intf: IRawBase> = class
    function Test: Intf;
  end;

  generic TComTest<Intf: IComBase> = class
    function Test: Intf;
  end;

function TRawTest.Test: Intf;
begin
  Result := Self as Intf;
end;

function TComTest.Test: Intf;
begin
  Result := Self as Intf;
end;

type
  TRawTestSub = specialize TRawTest<IRawSub>;
  TComTestSub = specialize TComTest<IComSub>;

begin
end.
