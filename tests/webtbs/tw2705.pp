{ Source provided for Free Pascal Bug Report 2705 }
{ Submitted by "Johannes Berg" on  2003-10-01 }
{ e-mail: johannes -at- sipsolutions -dot- de }
program i;

{$mode delphi}

type
  TClassA = class
  end;
  TClassB = class
    FA: TClassA;
    property a: TClassA read FA write FA default nil;
  end;

begin
end.
