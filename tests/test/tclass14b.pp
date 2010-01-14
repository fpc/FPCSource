{ %FAIL}
program tclass14b;

{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

type
  TSomeClass = class
  public
    class var
      FSomeField: Integer;
  published
    // class properties are not for sreaming therefore publishing them is not supported
    class property SomeField: Integer read FSomeField write FSomeField;
  end;

begin
end.
