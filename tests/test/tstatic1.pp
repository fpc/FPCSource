program tstatic1;
{$APPTYPE console}
{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

type
  TSomeClass = class
  public
    class procedure SomeClassMethod(A: Integer);
    class procedure SomeStaticMethod(A: Integer); static;
  end;

{ TSomeClass }

class procedure TSomeClass.SomeClassMethod(A: Integer);
begin
  WriteLn('TSomeClass.SomeClassMethod: ', A);
end;

// for now fpc requires 'static' modifiers also in the class implementation
class procedure TSomeClass.SomeStaticMethod(A: Integer); {$ifdef fpc} static; {$endif}
begin
  WriteLn('TSomeClass.SomeStaticMethod: ', A);
  SomeClassMethod(A + 1);
end;

begin
  TSomeClass.SomeStaticMethod(1);
end.