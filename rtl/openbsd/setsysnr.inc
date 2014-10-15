
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

  if (do_syscall(syscall_nr___sysctl,TSysParam(@mib), 2, TSysParam(@release[1]),
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

  if version >= 5005 then
    begin
      syscall_nr_waitpid := syscall_nr_wait4_55;
      syscall_nr_setitimer := syscall_nr_setitimer_55;
      syscall_nr_getitimer := syscall_nr_getitimer_55;
      syscall_nr_gettimeofday := syscall_nr_gettimeofday_55;
      syscall_nr_settimeofday := syscall_nr_settimeofday_55;
      syscall_nr_clock_gettime := syscall_nr_clock_gettime_55;
      syscall_nr_clock_settime := syscall_nr_clock_settime_55;
      syscall_nr_clock_getres := syscall_nr_clock_getres_55;
      syscall_nr_select := syscall_nr_select_55;
      syscall_nr_getrusage := syscall_nr_getrusage_55;
      syscall_nr_utimes := syscall_nr_utimes_55;
      syscall_nr_futimes := syscall_nr_futimes_55;
      syscall_nr_nanosleep := syscall_nr_nanosleep_55;
      syscall_nr_kevent := syscall_nr_kevent_55;
    end;
  if version>= 5004 then
    begin
      syscall_nr_statfs := syscall_nr_statfs_54;
      syscall_nr_fstatfs := syscall_nr_fstatfs_54;
      syscall_nr_fhstatfs := syscall_nr_fhstatfs_54;
    end
  else if version >= 4004 then
    begin
      syscall_nr_statfs := syscall_nr_statfs_44;
      syscall_nr_fstatfs := syscall_nr_fstatfs_44;
      syscall_nr_fhstatfs := syscall_nr_fhstatfs_44;
    end
  else if version < 2006 then
    begin
      syscall_nr_statfs := syscall_nr_statfs_20;
      syscall_nr_fstatfs := syscall_nr_fstatfs_20;
      syscall_nr_pipe := syscall_nr_pipe_20;
    end;

  if version>= 5005 then
    begin
      syscall_nr_stat := syscall_nr_stat_55;
      syscall_nr_fstat := syscall_nr_fstat_55;
      syscall_nr_lstat := syscall_nr_lstat_55;
      syscall_nr_fstatat := syscall_nr_fstatat_55;
    end
  else if version < 3006 then
    begin
      syscall_nr_stat := syscall_nr_stat_20;
      syscall_nr_fstat := syscall_nr_fstat_20;
      syscall_nr_lstat := syscall_nr_lstat_20;
      syscall_nr_fhstat := syscall_nr_fhstat_20;
    end;
  if version < 3005 then
    syscall_nr_sigaltstack := syscall_nr_sigaltstack_20;
  if version = 5001 then
    syscall_nr___tfork := syscall_nr___tfork_51
  else if version > 5001 then
    syscall_nr___tfork := syscall_nr___tfork_52;



end;
