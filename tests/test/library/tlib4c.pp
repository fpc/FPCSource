{ %target=win32,win64 }

{ The library of tlib4a.pp cannot have the threadvars of a new thread that
  the program of tlib4b.pp starts, so the process ends with run time error
  226 from that thread. This program runs tlib4b with its standard output and
  its standard error going to two files, and requires exit code 226 and the
  message once in the standard error file (WriteErrorsToStdErr is True by
  default), not in the other one; then runs it again with the argument
  stdout, which makes tlib4b have the library set its WriteErrorsToStdErr to
  False, and requires the message once in the standard output file instead
  (exit codes 10 higher). }
program tlib4c;

{$mode objfpc}

uses
  windows;

const
  { tlib4b ends within seconds; this stays well below the 60 seconds after
    which dotest ends the test itself }
  ChildTimeout = 20000;

var
  outname, errname: ansistring;

function LastError: ansistring;
begin
  str(GetLastError,result);
  result:=' (error '+result+')';
end;

procedure Fail(code: longint; const msg: ansistring);
begin
  writeln(msg);
  writeln('any captures made are kept: ',outname,', ',errname);
  halt(code);
end;

function CreateCapture(const name: ansistring): THandle;
var
  sa: TSecurityAttributes;
begin
  fillchar(sa,sizeof(sa),0);
  sa.nLength:=sizeof(sa);
  sa.bInheritHandle:=true;
  result:=CreateFileA(pansichar(name),GENERIC_WRITE,FILE_SHARE_READ,@sa,CREATE_ALWAYS,FILE_ATTRIBUTE_NORMAL,0);
  if result=INVALID_HANDLE_VALUE then
    Fail(1,'could not create '+name+LastError);
end;

function ReadCapture(const name: ansistring): ansistring;
var
  h: THandle;
  buf: array[0..1023] of ansichar;
  got: dword;
  len: longint;
begin
  result:='';
  h:=CreateFileA(pansichar(name),GENERIC_READ,FILE_SHARE_READ,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
  if h=INVALID_HANDLE_VALUE then
    Fail(1,'could not open '+name+LastError);
  repeat
    got:=0;
    if not ReadFile(h,buf,sizeof(buf),got,nil) then
      Fail(1,'could not read '+name+LastError);
    len:=length(result);
    setlength(result,len+longint(got));
    if got>0 then
      move(buf,result[len+1],got);
  until got=0;
  CloseHandle(h);
end;

{ runs tlib4b with arg as its command line argument and requires the message
  once in the capture of the stream it is expected on, and not in the other;
  base is added to the exit code of every failure after the start }
procedure RunChild(const child, arg: ansistring; tostdout: boolean; base: longint);
var
  si: TStartupInfo;
  pi: TProcessInformation;
  cmdline, outtext, errtext, expected, other: ansistring;
  hout, herr: THandle;
  code: dword;
begin
  hout:=CreateCapture(outname);
  herr:=CreateCapture(errname);
  fillchar(si,sizeof(si),0);
  si.cb:=sizeof(si);
  si.dwFlags:=STARTF_USESTDHANDLES;
  si.hStdInput:=GetStdHandle(STD_INPUT_HANDLE);
  si.hStdOutput:=hout;
  si.hStdError:=herr;
  cmdline:='"'+child+'" '+arg;
  if not CreateProcessA(pansichar(child),pansichar(cmdline),nil,nil,true,0,nil,nil,si,pi) then
    Fail(1,'tlib4b could not be started'+LastError);
  CloseHandle(hout);
  CloseHandle(herr);
  case WaitForSingleObject(pi.hProcess,ChildTimeout) of
    WAIT_OBJECT_0:
      ;
    WAIT_TIMEOUT:
      begin
        if not TerminateProcess(pi.hProcess,1) then
          Fail(base+4,'tlib4b was still running when the wait ran out, and could not be ended'+LastError);
        case WaitForSingleObject(pi.hProcess,5000) of
          WAIT_OBJECT_0:
            ;
          WAIT_TIMEOUT:
            Fail(base+4,'tlib4b was still running when the wait ran out, and did not end when told to');
        else
          Fail(base+4,'tlib4b was still running when the wait ran out, and waiting for its end failed'+LastError);
        end;
        Fail(base+4,'tlib4b was still running when the wait ran out');
      end;
  else
    Fail(base+4,'waiting for tlib4b failed'+LastError);
  end;
  if not GetExitCodeProcess(pi.hProcess,code) then
    Fail(base+4,'the exit code of tlib4b could not be read'+LastError);
  CloseHandle(pi.hThread);
  CloseHandle(pi.hProcess);
  writeln('tlib4b ',arg,' ended with exit code ',code);

  outtext:=ReadCapture(outname);
  errtext:=ReadCapture(errname);
  writeln('its standard output: ',outtext);
  writeln('its standard error: ',errtext);
  if tostdout then
    begin
      expected:=outtext;
      other:=errtext;
    end
  else
    begin
      expected:=errtext;
      other:=outtext;
    end;
  if code<>226 then
    Fail(base+2,'the exit code is not 226');
  if pos('Runtime error 226',expected)=0 then
    Fail(base+3,'the message is not on the stream WriteErrorsToStdErr selects');
  if pos('Runtime error 226',copy(expected,pos('Runtime error 226',expected)+1,length(expected)))<>0 then
    Fail(base+6,'the message is written more than once');
  if pos('Runtime error 226',other)<>0 then
    Fail(base+5,'the message is on the other stream');
  DeleteFileA(pansichar(outname));
  DeleteFileA(pansichar(errname));
end;

var
  dir: ansistring;
  i: longint;
begin
  { an ansistring: objpas's ParamStr in this mode }
  dir:=ParamStr(0);
  i:=length(dir);
  while (i>0) and not (dir[i] in ['\','/',':']) do
    dec(i);
  setlength(dir,i);
  outname:=dir+'tlib4c.out';
  errname:=dir+'tlib4c.err';
  RunChild(dir+'tlib4b.exe','',false,0);
  RunChild(dir+'tlib4b.exe','stdout',true,10);
end.
