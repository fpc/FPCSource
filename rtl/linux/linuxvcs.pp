unit linuxvcs;

{*****************************************************************************}
                                   interface
{*****************************************************************************}

const vcs_device:shortint=-1;

function try_grab_vcsa:boolean;

{*****************************************************************************}
                                 implementation
{*****************************************************************************}

uses baseunix,strings;

function try_grab_vcsa_in_path(path:Pchar;len:cardinal):boolean;

const  grab_vcsa='/grab_vcsa';
       grab_vcsa_s:array[1..length(grab_vcsa)] of char=grab_vcsa;

var p:Pchar;
    child:Tpid;
    status:cint;
    pstat:stat;

begin
  getmem(p,len+length(grab_vcsa)+1);
  move(path^,p^,len);
  move(grab_vcsa_s,(p+len)^,length(grab_vcsa));
  (p+len+length(grab_vcsa))^:=#0;
  {Check if file exists.}
  if fpstat(p,pstat)<>0 then
    begin
      try_grab_vcsa_in_path:=false;
      exit;
    end;
  child:=fpfork;
  if child=0 then
    begin
      fpexecve(p,nil,nil);
      halt(255); {fpexec must have failed...}
    end;
  fpwaitpid(child,status,0);
  try_grab_vcsa_in_path:=status=0; {Return true if success.}
  freemem(p);
end;


function try_grab_vcsa:boolean;

{If we cannot open /dev/vcsa0-31 it usually because we do not have
 permission. At login the owner of the tty you login is set to yourself.

 This is not done for vcsa, which is kinda strange as vcsa is revoke from
 you when you log out. We try to call a setuid root helper which chowns
 the vcsa device so we can get access to the screen buffer...}

var path,p:Pchar;

begin
  try_grab_vcsa:=false;
  path:=fpgetenv('PATH');
  if path=nil then
    exit;
  p:=strscan(path,':');
  while p<>nil do
    begin
      if try_grab_vcsa_in_path(path,p-path) then
        begin
          try_grab_vcsa:=true;
          exit;
        end;
      path:=p+1;
      p:=strscan(path,':');
    end;
  if try_grab_vcsa_in_path(path,strlen(path)) then
    exit;
end;


procedure detect_linuxvcs;

var f:text;
    c:char;
    pid,ppid,dummy:integer;
    device:longint;
    s:string[15];

begin
  {Extremely aggressive VCSA detection. Works even through Midnight
   Commander. Idea from the C++ Turbo Vision project, credits go
   to Martynas Kunigelis <algikun@santaka.sc-uni.ktu.lt>.}
  pid:=fpgetpid;
  repeat
    str(pid,s);
    assign(f,'/proc/'+s+'/stat');
    reset(f);
    if ioresult<>0 then
      break;
    read(f,dummy);
    read(f,c);
    repeat
      read(f,c);
    until c=' ';
    repeat
      read(f,c);
    until c=' ';
    ppid:=pid;
    read(f,pid);
    read(f,dummy);
    read(f,dummy);
    read(f,device);
    close(f);
    if device and $ffffffc0=$00000400 then {/dev/tty*}
      begin
        vcs_device:=device and $3f;
        break;
      end;
   until (device=0) {Not attached to a terminal, i.e. an xterm.}
      or (pid=-1)
      or (ppid=pid);
end;

begin
  {Put in procedure because there are quite a bit of variables which are made
   temporary this way.}
  detect_linuxvcs;
end.
