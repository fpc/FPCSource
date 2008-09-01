{$mode objfpc}
program test;

type
  TStringArray = array of String;

  TBug = class
  private
    fSA: TStringArray;
  published
    property SA: TStringArray read fSA write fSA;
  end;

begin

end.
