unit ucustomattr14a;

{$mode objfpc}{$H+}

interface

type
  TTestAttribute = class(TCustomAttribute)
    constructor Create;
  end;

  TTest2Attribute = class(TCustomAttribute)
    constructor Create(const aStr: String);
  end;

implementation

constructor TTestAttribute.Create;
begin

end;

constructor TTest2Attribute.Create(const aStr: String);
begin

end;

end.

