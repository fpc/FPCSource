{
    $Id$
    Copyright (c) 1998-2000 by Pavel Ozerski and Pierre Muller

    This program implements support post processing
    for the (i386) Win32 target

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}

program post_process_win32_executable;


uses
  globtype,globals,t_win32,strings;

const
  execinfo_f_cant_open_executable='Cannot open file ';
  execinfo_x_codesize='Code size: ';
  execinfo_x_initdatasize='Size of Initialized Data: ';
  execinfo_x_uninitdatasize='Size of Uninitialized Data: ';
  execinfo_f_cant_process_executable='Cannot process file ';
  execinfo_x_stackreserve='Size of Stack Reserve: ';
  execinfo_x_stackcommit='Size of Stack Commit: ';

var
  verbose:longbool;
  ii,jj,p:longint;
  x:single;
  code:integer;

procedure Message1(const info,fn:string);
var
  e:longbool;
begin
  e:=pos('Cannot',info)=1;
  if verbose or e then
   writeln(info,fn);
  if e then
   halt(1);
end;

var
  l:tlinkerwin32;
  fn,s:string;
  isDll:boolean;

function GetSwitchValue(const key,shortkey,default:string;const PossibleValues:array of pchar):string;
var
  i,j,k:longint;
  x:double;
  s1,s2:string;
  code:integer;
procedure Error;
  begin
   writeln('Error: unrecognized option ',paramstr(i),' ',s1);
   halt(1);
  end;
begin
  for i:=1 to paramcount do
   if(paramstr(i)=key)or(paramstr(i)=shortkey)then
    begin
     s1:=paramstr(succ(i));
     for j:=0 to high(PossibleValues)do
      begin
       s2:=strpas(PossibleValues[j]);
       if(length(s2)>1)and(s2[1]='*')then
        case s2[2]of
         'i':
          begin
           val(s1,k,code);
           if code<>0 then
            error;
           GetSwitchValue:=s1;
           exit;
          end;
         'r':
          begin
           val(s1,x,code);
           if code<>0 then
            error;
           GetSwitchValue:=s1;
           exit;
          end;
         's':
          begin
           GetSwitchValue:=s1;
           exit;
          end;
        end
       else if s1=s2 then
        begin
         GetSwitchValue:=s1;
         exit;
        end;
      end;
     error;
    end;
  GetSwitchValue:=default;
end;
procedure help_info;
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
  writeln('-i | --input <file>              - set input file;');
  writeln('-m | --subsystem <console | gui> - set Win32 subsystem;');
  writeln('-s | --stack <size>              - set stack size;');
  writeln('-t | --type <exe | dll>          - define binary type;');
  writeln('-V | --version <n.n>             - set image version;');
  writeln('-v | --verbose                   - show info while processing;');
  writeln('-h | --help | -?                 - show this screen');
  halt;
end;

begin
aktglobalswitches:=[];
verbose:=false;
if paramcount=0 then
  help_info;
for ii:=1 to paramcount do
  if(paramstr(ii)='-h')or(paramstr(ii)='--help')or(paramstr(ii)='-?')then
   help_info
  else if(paramstr(ii)='-v')or(paramstr(ii)='--verbose')then
   begin
    verbose:=true;
    break;
   end;
fn:=GetSwitchValue('--input','-i','',['*s']);
val(GetSwitchValue('--stack','-s','33554432',['*i']),stacksize,code);
                                                 {value from
                                                 systems.pas
                                                 for Win32 target}

s:=GetSwitchValue('--subsystem','-m','console',['gui','console']);
if s='gui' then
  apptype:=at_GUI
else
  apptype:=at_cui;

dllversion:=GetSwitchValue('--version','-V','1.0',['*r']);
{ val(dllversion,x,code);
dllmajor:=trunc(x);
dllminor:=trunc(frac(x)*10);
 This does not work for 1.12 !! PM }
p:=pos('.',dllversion);
if p=0 then
  begin
    dllminor:=0;
    val(dllversion,dllmajor,code);
  end
else
  begin
    val(copy(dllversion,1,p-1),dllmajor,code);
    val(copy(dllversion,p+1,255),dllminor,code);
  end;

isDll:=GetSwitchValue('--type','-t','exe',['exe','dll'])='dll';
{ if isDLL then
  aktglobalswitches:=[cs_link_extern];
  no because otherwise you don't change anything to a dll !!
  by the way why not simply use the suffix ?? PM }
l.init;
l.PostProcessExecutable(fn,isdll);

end.