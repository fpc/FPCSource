{$S+}

{ Program to check that an infinite recursion
  does generate a RTE ... }

{$R-}
{ make that recursion really infinite
  needs that range check is disabled }

const
  level : longint = 0;

function inf_rec(x : longint) : longint;

begin
  inc(level);
  inf_rec:=x+inf_rec(x-1);
end;


const
  saveexit : pointer = nil;
  x : longint = 0;

{$S-}
{ the stack overflowed already so don't do much here and depend on stack_margin }
procedure stack_check_exit;

begin
  exitproc:=saveexit;
  if errorcode<>0 then
    begin
      Writeln('An error occured at level ',level);
      if errorcode=202 then
        begin
          Writeln('Stack overflow correctly handled');
          erroraddr:=nil;
          errorcode:=0;
          exitcode:=0;
        end
      else if errorcode=216 then
        begin
          Writeln('RTL returns an RTE 216 on stack overflow');
          Writeln('Not perfect, but acceptable');
          erroraddr:=nil;
          errorcode:=0;
          exitcode:=0;
        end;
    end
  else
    begin
      exitcode:=1;
      errorcode:=1;
    end;
  exitproc:=saveexit;
end;

begin
  saveexit:=exitproc;
  exitproc:=@stack_check_exit;
  x:=inf_rec(5000);
end.
