{ %fail }

{ Source provided for Free Pascal Bug Report 4569 }
{ Submitted by "Vincent Snijders" on  2005-12-06 }
{ e-mail: vsnijders@quicknet.nl }
program fpcdos;

{$mode objfpc}

type
  TMyClassB = class;
  TMyClassC = class;

  TMyClassB = class(TMyClassC)
    procedure DoSomething; override;
  end;

  TMyClassC = class(TMyClassB)
    procedure DoSomething; override;
  end;

begin
end.
