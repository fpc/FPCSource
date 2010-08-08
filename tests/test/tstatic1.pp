program tstatic1;
{$APPTYPE console}
{$ifdef fpc}
  {$mode delphi}{$H+}
{$endif}

type
  TSomeClass = class
  private
    class var 
      FSomethingStatic: Integer;
  public
    class procedure SomeClassMethod(A: Integer);
    class procedure SomeStaticMethod(A: Integer); inline; static;
  end;

{ TSomeClass }

class procedure TSomeClass.SomeClassMethod(A: Integer);
begin
  WriteLn('TSomeClass.SomeClassMethod: ', A);
end;

// for now fpc requires 'static' modifiers also in the class implementation
class procedure TSomeClass.SomeStaticMethod(A: Integer);
begin
  WriteLn('TSomeClass.SomeStaticMethod: ', A);
  WriteLn('TSomeClass.FSomethingStatic: ', FSomethingStatic);
  SomeClassMethod(A + 1);
end;

begin
  TSomeClass.FSomethingStatic := 4;
  TSomeClass.SomeStaticMethod(1);
end.