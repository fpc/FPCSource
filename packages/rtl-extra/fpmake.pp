{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_rtl_extra(const ADirectory: string);

Const 
  // All Unices have full set of KVM+Crt in unix/ except QNX which is not
  // in workable state atm.
  UnixLikes = AllUnixOSes -[QNX]; // qnx never was active in 2.x afaik

  UComplexOSes  = [amiga,emx,gba,go32v2,morphos,msdos,nativent,nds,netware,netwlibc,os2,watcom,wii,wince,win32,win64]+UnixLikes;
  MatrixOSes	= [amiga,emx,gba,go32v2,morphos,msdos,nativent,nds,netware,netwlibc,os2,wii,win32,win64,wince]+UnixLikes;
  ObjectsOSes   = [amiga,emx,gba,go32v2,morphos,msdos,netware,netwlibc,os2,win32,win64,wince]+UnixLikes;
  WinsockOSes   = [win32,win64,wince,os2,emx,netware,netwlibc];
  WinSock2OSes  = [win32,win64,wince];
  // sockets of  morphos is implemented, but not active
  SocketsOSes   = UnixLikes+[netware,netwlibc,os2,wince,win32,win64];
  gpmOSes	= [Linux,Android];
  AllTargetsextra = ObjectsOSes + UComplexOSes + MatrixOSes;

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('rtl-extra');
    P.Directory:=ADirectory;
    P.Version:='2.7.1';
    P.Author := 'FPC core team';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.OSes:=AllTargetsextra;
    P.Email := '';
    P.Description := 'Rtl-extra, RTL not needed for bootstrapping';
    P.NeedLibC:= false;

    P.SourcePath.Add('src/inc');
    P.SourcePath.Add('src/$(OS)');
    P.SourcePath.Add('src/darwin',[iphonesim]);
    P.SourcePath.Add('src/unix',AllUnixOSes);
    P.SourcePath.Add('src/os2commn',[os2,emx]);
    P.SourcePath.Add('src/netwcomn',[netware,netwlibc]);
    P.SourcePath.Add('src/win',[win32,win64]);

    P.IncludePath.Add('src/inc');
    P.IncludePath.Add('src/unix',AllUnixOSes);
    P.IncludePath.Add('src/$(OS)');
    P.IncludePath.Add('src/darwin',[iphonesim]);

    T:=P.Targets.AddUnit('ucomplex.pp',UComplexOSes);

    T:=P.Targets.AddUnit('objects.pp',ObjectsOSes);

    T:=P.Targets.AddUnit('matrix.pp',MatrixOSes);
    with T.Dependencies do
     begin
       AddInclude('mvecimp.inc');
       AddInclude('mmatimp.inc');
     end;
    T:=P.Targets.AddUnit('winsock.pp',WinSockOSes);
    with T.Dependencies do
     begin
       AddInclude('qos.inc',[netware,netwlibc]);
       AddInclude('netwsockh.inc',[netware,netwlibc]);
     end;
    T:=P.Targets.AddUnit('gpm.pp',gpmOSes);
    with T.Dependencies do
      AddUnit('sockets');
    T:=P.Targets.AddUnit('sockets.pp',SocketsOSes);
    with T.Dependencies do
     begin
       addinclude('socketsh.inc');
       addinclude('sockets.inc');
       addinclude('sockovl.inc');
     end; 
  end
end;
 
{$ifndef ALLPACKAGES}
begin
  add_rtl_extra('');
  Installer.Run;
end.
{$endif ALLPACKAGES}

