{ %norun }

{$mode objfpc}
{$h+}
{$inline on}
unit tw12404;

interface

Type
  TMyClass = Class(TObject)
  Private
    FPos : integer;
    FChar : PChar;
    Function MyFunc : Char; inline;
    Function MyOtherFunction : Integer;
  end;

implementation

Function TMyClass.MyFunc : Char; inline;

begin
  Inc(FPos);
  Inc(FChar);
  Result:=FChar^;
end;

Function TMyClass.MyOtherFunction : Integer;

Var
  C : Char;

begin
  C:=MyFunc;
end;

end.

            
