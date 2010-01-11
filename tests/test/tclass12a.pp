program tclass12a;
{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TSomeClass = class
  strict private
    const
      PrivateConst = 3.14;
  public
    class procedure WritePrivateConst; static;
  end;

  class procedure TSomeClass.WritePrivateConst;
  begin
    WriteLn(PrivateConst);
  end;

begin
  TSomeClass.WritePrivateConst;
end.