{$APPTYPE CONSOLE}
program FPimpdef;
{$DEFINE STANDALONE}
uses
 DOS,
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
  writeln('-i | --input     <file> - set input file;');
  writeln('-o | --output    <file> - set output .def file');
  writeln('-l | --library   <file> - set output static library');
  writeln('-s | --assembler <name> - use <name> for assembler (default asw)');
  writeln('-r | --archiver  <name> - use <name> for archiver (default arw)');
  writeln('-h | --help             - show this screen');
  halt;
end;
{$ifndef UNIX}
procedure AddExt(var s:string);
 var
  s1:string;
  i:longint;
 begin
  s1:=copy(s,length(s)-3,4);
  for i:=1 to length(s1)do
   s1[i]:=upcase(s1[i]);
  if s1<>'.EXE'then
   s:=s+'.EXE';
 end;
{$endif}
var
 EnvPath:string;
begin
binname:=GetOption('-i','--input');
if(binname='')or(Ofound('-h','--help')>0)then
  help_info;
 as_name:=GetOption('-s','--assembler');
 if as_name='' then
  as_name:='as';
 ar_name:=GetOption('-r','--archiver');
 if ar_name='' then
  ar_name:='ar';
{$ifndef UNIX}
 AddExt(as_name);
 AddExt(ar_name);
{$endif}
 EnvPath:=GetEnv('Path');
 if EnvPath='' then
  EnvPath:=GetEnv('PATH');
 as_name:=FSearch(as_name,EnvPath);
 ar_name:=FSearch(ar_name,EnvPath);
if not makedef(binname,GetOption('-o','--output'),GetOption('-l','--library'))then
  begin
   writeln('Export names not found');
   halt(1);
  end;
end.
