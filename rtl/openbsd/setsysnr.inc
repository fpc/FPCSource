
{$ifdef FPC_COMPILING_SYSCALL_UNIT}

{ these variables are used inside osysc.inc file
  but needed here also if compiling syscall unit. }
{$i errno.inc}
{$i ptypes.inc}

{$endif FPC_COMPILING_SYSCALL_UNIT}

{$ifndef FPC_IS_SYSTEM}

function geterrno:longint; external name 'FPC_SYS_GETERRNO';
procedure seterrno(err:longint); external name 'FPC_SYS_SETERRNO';

{$endif FPC_IS_SYSTEM}

procedure SetSyscallNumbers;

Var
  mib  : array[0..1] of cint;
  release : shortstring;
  len  : size_t;
  oerrno : cint;
  err : word;
  point, c : char;
  i, version_major, version_minor, version : cint;
Begin
  mib[0] := 1{ CTL_KERN};
  mib[1] := 2{KERN_OSRELEASE};
  len    := sizeof (release) - 1;
  oerrno := geterrno;

  if (do_syscall(syscall_nr_sysctl,TSysParam(@mib), 2, TSysParam(@release[1]),
       TSysParam(@len), TSysParam(NIL), 0) = -1) Then
    Begin
      if (geterrno = ESysENOMEM) Then
        seterrno(oerrno);
      version_major:=3;
    version_minor:=9;
    End
  else
    begin
      setlength(release,len);
      i:=0;
      while (i < len) do
        begin
          if (release[i]='.') then
            break;
          inc (i);
        end;
      setlength(release,i-1);
      val (release, version_major, err);
      if err<> 0 then
        version_major:=3;
      setlength(release,len);
      release:=copy(release,i+1,len);
      val (release, version_minor, err);
      if err<> 0 then
        version_minor:=9;
    end;

  version:=version_major * 1000 + version_minor;

  { version-specific checks and setting of compatibility flags can be inserted here }
end;
