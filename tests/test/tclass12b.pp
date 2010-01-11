{ %FAIL}
program tclass12b;
{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TSomeClass = class
  strict private
    const
      PrivateConst = 3.14;
  end;

  TAnotherClass = class(TSomeClass)
  public
    class procedure WritePrivateConst; static;
  end;

  class procedure TAnotherClass.WritePrivateConst;
  begin
    WriteLn(PrivateConst)
  end;

begin
  TAnotherClass.WritePrivateConst
end.
