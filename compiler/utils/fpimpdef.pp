program FPimpdef;
uses
ImpDef;
var
binname:string;
function Ofound(const short,full:string):longint;
var
  i:longint;
begin
  Ofound:=-1;
  for i:=1 to ParamCount do
   if(paramstr(i)=short)or(paramstr(i)=full)then
    begin
     Ofound:=i;
     exit;
    end;
end;
function GetOption(const short,full:string):string;
var
  i:longint;
begin
  i:=Ofound(short,full);
  if i>0 then
   GetOption:=paramstr(succ(i))
  else
   GetOption:='';
end;
procedure help_info;
var
  fn:string[255];
  jj:cardinal;
begin
  fn:=paramstr(0);
  for jj:=length(fn)downto 1 do
   if fn[jj] in [':','\','/']then
    begin
     fn:=copy(fn,succ(jj),255);
     break;
    end;
  writeln('Usage: ',fn,' [options]');
  writeln('Options:');
  writeln('-i | --input  <file> - set input file;');
  writeln('-o | --output <file> - set output file');
  writeln('-h | --help          - show this screen');
  halt;
end;
begin
binname:=GetOption('-i','--input');
if(binname='')or(Ofound('-h','--help')>0)then
  help_info;
if not makedef(binname,GetOption('-o','--output'))then
  begin
   writeln('Export names not found');
   halt(1);
  end;
end.