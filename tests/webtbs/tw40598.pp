{ %RESULT=202 }
{ %opt=-gl -Ct }

{$mode objfpc}

type
  TForm1 = class
    procedure Button1Click(Sender: TObject);
  end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  writeln(hexstr(stackbottom));
  writeln(hexstr(sptr));
  Button1Click(self);
end;

var
  Form1 : TForm1;
{$ifdef linux}
  limits : TRLimit;
{$endif linux}

begin
{$ifdef linux}
  FpGetRLimit(RLIMIT_STACK, @limits);
  writeln('Cur: ',limits.rlim_cur);
  writeln('Max: ',limits.rlim_max);
  writeln('StackLength: ',StackLength);
{$endif linux}
  Form1:=TForm1.Create;
  Form1.Button1Click(nil);
end.
