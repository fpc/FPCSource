{ Source provided for Free Pascal Bug Report 2729 }
{ Submitted by "marco (the gory bugs department)" on  2003-10-09 }
{ e-mail:  }
{$mode delphi}

type  tbla= class(tobject)
              class function bla:tbla;
              end;

class function tbla.bla:tbla;

begin
 result:=Create;
end;

begin
end.
