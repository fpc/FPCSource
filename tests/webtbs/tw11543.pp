unit test;
{$ifdef fpc}{$mode objfpc}{$endif}

interface

type
  TSetOfChar = set of char;

  ttestclass = class(tobject)
    procedure p(const a: array of WideChar); overload;
    procedure p(const a: TSetOfChar); overload;
  end;

implementation

procedure ttestclass.p(const a: array of WideChar);
begin
end;

procedure ttestclass.p(const a: TSetOfChar);
begin
end;

end.