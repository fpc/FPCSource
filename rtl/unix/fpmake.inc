Procedure ApplyUnixTargets(Installer : TInstaller);

Var
  T : TTarget;

begin
  With Installer,Targets do
    begin
    T:=AddUnit('unix/syscall.pp');
    T:=AddUnit('unix/unixtype.pp');
    T:=AddUnit('unix/baseunix.pp');
    T.Dependencies.Add('unixtype');
    T:=AddUnit('unix/errors.pp');
    T.Dependencies.Add('strings');
    T:=AddUnit('unix/unix.pp');
    T.Dependencies.Add('baseunix');
    T.Dependencies.Add('unixtype');
    T.Dependencies.Add('strings');
    T:=AddUnit('unix/terminfo.pp');
    T.Dependencies.Add('baseunix');
    T:=AddUnit('unix/linux.pp');
    T:=AddUnit('unix/oldlinux.pp');
    T:=AddUnit('unix/unixutil.pp');
    if Defaults.CPU=i386 then
      T:=AddUnit('unix/x86');
    With Targets['sysutils'].dependencies do
      begin
      add('unix');
      add('errors');
      Add('unixtype');
      Add('baseunix');
      end;
    With Targets['Dos'].dependencies do
      begin
      Add('strings');
      Add('unix');
      Add('baseunix');
      Add('syscall');
      end;  
    end;
end;  