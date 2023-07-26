// searched from last to first !
{$NAMESPACES nt2,nt}

uses a;

var
  which : string;
begin
  which:=GetIt;
  if (which<>'a1') then
    begin
    Writeln('Wrong namespace used, expected a1, but got: ',which);
    Halt(1);
    end;
end.