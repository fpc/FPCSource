{ Source provided for Free Pascal Bug Report 2736 }
{ Submitted by "Johannes Berg" on  2003-10-13 }
{ e-mail: bugs@johannes.sipsolutions.de }
program test;
{$MODE delphi}

type
  TTest1 = class
    FA: Integer;
  private
    property a: Integer read FA write FA;
  end;
  TTest2 = class(TTest1)
    procedure b(a: Integer);
  end;

procedure TTest2.b(a: Integer);
begin
end;

begin
end.
