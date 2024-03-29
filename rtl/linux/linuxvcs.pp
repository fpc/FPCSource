{$IFNDEF FPC_DOTTEDUNITS}
unit linuxvcs;
{$ENDIF FPC_DOTTEDUNITS}

{*****************************************************************************}
                                   interface
{*****************************************************************************}

const vcs_device:shortint=-1;

function try_grab_vcsa:boolean;

{*****************************************************************************}
                                 implementation
{*****************************************************************************}

{$IFDEF FPC_DOTTEDUNITS}
uses UnixApi.Base,System.Strings;
{$ELSE FPC_DOTTEDUNITS}
uses baseunix,strings;
{$ENDIF FPC_DOTTEDUNITS}

function try_grab_vcsa_in_path(path:PAnsiChar;len:cardinal):boolean;

const  grab_vcsa='/grab_vcsa';
       grab_vcsa_s:array[1..length(grab_vcsa)] of AnsiChar=grab_vcsa;

var p:PAnsiChar;
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

var path,p:PAnsiChar;

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
    fields:array [0..60] of int64;
    fieldct,i:integer;
    pid,ppid:longint;
    magnitude:int64;
    s:string[15];
    statln:ansistring;

begin
  {Extremely aggressive VCSA detection. Works even through Midnight
   Commander. Idea from the C++ Turbo Vision project, credits go
   to Martynas Kunigelis <algikun@santaka.sc-uni.ktu.lt>.}
  pid:=fpgetpid;
  repeat
    str(pid,s);
    assign(f, '/proc/'+s+'/stat');
    {$I-}
    reset(f);
    {$I+}
    if ioresult<>0 then
      break;
    readln(f, statln);
    close(f);
    magnitude := 1;
    fieldct := 0;
    fields[fieldct] := 0;
    for i := high(statln) downto low(statln) do
      begin
{$push}{$R-} {$Q-}
        case statln[i] of
          '-': magnitude := -1;
          '0'..'9': begin
            fields[fieldct] := fields[fieldct]
                               + (magnitude * (ord(statln[i]) - ord('0')));
            magnitude := magnitude * 10;
          end;
{$pop}
          ' ': begin
            magnitude := 1;
            fieldct := fieldct + 1;
            fields[fieldct] := 0;
          end;
        else
          break;
        end;
      end;
    ppid := pid;
    pid := fields[fieldct - 1];
    if (fields[fieldct - 4] and $ffffffc0) = $00000400 then {/dev/tty*}
      begin
        vcs_device:=fields[fieldct - 4] and $3f;
        break;
      end;
  until (fields[fieldct - 4]=0) {Not attached to a terminal, i.e. an xterm.}
        or (pid=-1)
        or (ppid=pid);
end;

begin
  {Put in procedure because there are quite a bit of variables which are made
   temporary this way.}
  detect_linuxvcs;
end.
