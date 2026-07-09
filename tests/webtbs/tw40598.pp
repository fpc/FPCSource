{ %RESULT=202 }
{ %opt=-gl -Ct -Oonostackframe }

{$mode objfpc}

{$ifdef linux}
uses
  baseunix;
{$endif linux}

type
  TForm1 = class
    function Button1Click(Sender: TObject): ptrint;
  end;

function TForm1.Button1Click(Sender: TObject): ptrint;
var
  l: ptrint;
begin
  writeln(hexstr(stackbottom));
  l := ptrint(sptr);
  writeln(hexstr(sptr));
  // prevent an endless loop due to tail call optimization
  result := l + Button1Click(self) + ptrint(sptr);
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
  writeln(Form1.Button1Click(nil));
end.
