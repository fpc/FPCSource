{$mode tp}
type ProcType    = procedure(s:string);
     GetProcType = function(s:string;var Proc:ProcType):boolean;

var  ProcVar    : ProcType;
     GetProcVar : GetProcType;

procedure Default(s:string);

begin
  writeln('This is Default:',s);
end;

procedure Proc1(s:string);

begin
  writeln('This is Proc1:',s);
end;

procedure Proc2(s:string);

begin
  writeln('This is Proc2:',s);
end;

function GetProc(s:string;var ProcVar:ProcType):boolean;

begin
  if s='Proc1' then begin
    ProcVar:=Proc1;
    GetProc:=true;
  end
  else
    if s='Proc2' then begin
      ProcVar:=Proc2;
      GetProc:=true;
    end
    else begin
      ProcVar:=Default;
      GetProc:=false;
    end;
end;

begin
  GetProcVar:=GetProc;
  if GetProcVar('Proc1',ProcVar) then
    ProcVar('ok')
  else
    halt(1);
  if GetProcVar('Proc2',ProcVar) then
    ProcVar('ok')
  else
    halt(1);
  if GetProcVar('xyz',ProcVar) then
    halt(1)
  else
   writeln('ok');
end.
