{ %fail }

{ Source provided for Free Pascal Bug Report 4569 }
{ Submitted by "Vincent Snijders" on  2005-12-06 }
{ e-mail: vsnijders@quicknet.nl }
program fpcdos;

{$mode objfpc}

type
  TMyClassA = class;

  TMyClassA = class(TMyClassA)
    procedure DoSomething; override;
  end;

begin
end.
