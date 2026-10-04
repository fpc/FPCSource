{ %target=win32 }

{ The main thread's threadvars cannot be had when the program of tlib5a.pp
  starts, so the run time error is reported before the RTL has set up its
  standard handles, and the report has to ask Windows for the handle. This
  program runs tlib5a with its standard output and its standard error going to
  two files, and requires exit code 226 and the message once in the standard
  error file (WriteErrorsToStdErr is True by default), not in the other one. }
program tlib5b;

{$mode objfpc}

uses
  windows;

const
  { tlib5a ends as soon as it starts; this stays well below the 60 seconds
    after which dotest ends the test itself }
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

var
  si: TStartupInfo;
  pi: TProcessInformation;
  dir, child, outtext, errtext: ansistring;
  hout, herr: THandle;
  code: dword;
  i: longint;
begin
{$ifndef FPC_USE_TLS_DIRECTORY}
  { without a TLS directory a program's main thread uses the threadvars in its
    image until it starts a thread, so tlib5a starts normally and there is
    nothing to test }
  writeln('skipped: the compiler makes no TLS directory');
  exit;
{$endif FPC_USE_TLS_DIRECTORY}
  { an ansistring: objpas's ParamStr in this mode }
  dir:=ParamStr(0);
  i:=length(dir);
  while (i>0) and not (dir[i] in ['\','/',':']) do
    dec(i);
  setlength(dir,i);
  child:=dir+'tlib5a.exe';
  outname:=dir+'tlib5b.out';
  errname:=dir+'tlib5b.err';

  hout:=CreateCapture(outname);
  herr:=CreateCapture(errname);
  fillchar(si,sizeof(si),0);
  si.cb:=sizeof(si);
  si.dwFlags:=STARTF_USESTDHANDLES;
  si.hStdInput:=GetStdHandle(STD_INPUT_HANDLE);
  si.hStdOutput:=hout;
  si.hStdError:=herr;
  if not CreateProcessA(pansichar(child),nil,nil,nil,true,0,nil,nil,si,pi) then
    Fail(1,'tlib5a could not be started'+LastError);
  CloseHandle(hout);
  CloseHandle(herr);
  case WaitForSingleObject(pi.hProcess,ChildTimeout) of
    WAIT_OBJECT_0:
      ;
    WAIT_TIMEOUT:
      begin
        if not TerminateProcess(pi.hProcess,1) then
          Fail(4,'tlib5a was still running when the wait ran out, and could not be ended'+LastError);
        case WaitForSingleObject(pi.hProcess,5000) of
          WAIT_OBJECT_0:
            ;
          WAIT_TIMEOUT:
            Fail(4,'tlib5a was still running when the wait ran out, and did not end when told to');
        else
          Fail(4,'tlib5a was still running when the wait ran out, and waiting for its end failed'+LastError);
        end;
        Fail(4,'tlib5a was still running when the wait ran out');
      end;
  else
    Fail(4,'waiting for tlib5a failed'+LastError);
  end;
  if not GetExitCodeProcess(pi.hProcess,code) then
    Fail(4,'the exit code of tlib5a could not be read'+LastError);
  CloseHandle(pi.hThread);
  CloseHandle(pi.hProcess);
  writeln('tlib5a ended with exit code ',code);

  outtext:=ReadCapture(outname);
  errtext:=ReadCapture(errname);
  writeln('its standard output: ',outtext);
  writeln('its standard error: ',errtext);
  if code<>226 then
    Fail(2,'the exit code is not 226');
  if pos('Runtime error 226',errtext)=0 then
    Fail(3,'the message is not on standard error');
  if pos('Runtime error 226',copy(errtext,pos('Runtime error 226',errtext)+1,length(errtext)))<>0 then
    Fail(6,'the message is written more than once');
  if pos('Runtime error 226',outtext)<>0 then
    Fail(5,'the message is on standard output');
  DeleteFileA(pansichar(outname));
  DeleteFileA(pansichar(errname));
end.
