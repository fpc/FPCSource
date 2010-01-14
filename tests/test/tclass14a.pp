{ %FAIL}
program tclass14a;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TSomeClass = class
  public
    class var
      FSomeField: Integer;
    // class properties are not for sreaming therefore 'stored' is not supported
    class property SomeField: Integer read FSomeField write FSomeField stored False;
  end;

begin
end.
