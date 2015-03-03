{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_rtl_objpas(const ADirectory: string);

Const 
  // All Unices have full set of KVM+Crt in unix/ except QNX which is not
  // in workable state atm.
  UnixLikes = AllUnixOSes -[QNX]; // qnx never was active in 2.x afaik

//  AllUnixOSes  = [Linux,FreeBSD,NetBSD,OpenBSD,Darwin,QNX,BeOS,Solaris,Haiku,iphonesim,aix,Android];
//    unixlikes-[beos];
// 
  StrUtilsOSes  = [amiga,aros,emx,gba,go32v2,msdos,nds,netware,wince,morphos,nativent,os2,netwlibc,win32,win64]+UnixLikes;
  VarUtilsOSes  = [amiga,aros,emx,gba,go32v2,msdos,nds,netware,wince,morphos,nativent,os2,netwlibc,watcom,wii,win32,win64]+UnixLikes;
  ConvUtilsOSes = [nativent,netware,netwlibc,win32,win64,wince]+UnixLikes-[BeOS];
  ConvUtilOSes  = [Go32v2,msdos,os2,emx];
  DateUtilsOSes = [Amiga,aros,gba,morphos,nativent,nds,netware,netwlibc,wii,win32,win64,wince]+UnixLikes;
  DateUtilOSes  = [Go32v2,msdos,os2,emx];
  StdConvsOSes  = [NativeNT,Win32,win64,os2,msdos,go32v2]+UnixLikes-[BeOS];
  FmtBCDOSes    = [amiga,aros,emx,gba,go32v2,morphos,msdos,nativent,nds,netware,netwlibc,os2,win32,win64,wince]+UnixLikes;
  VariantsOSes  = [amiga,aros,emx,gba,go32v2,morphos,msdos,nativent,nds,netware,netwlibc,os2,watcom,wii,win32,win64,wince]+UnixLikes;
  AllTargetsObjPas = DateUtilsOses +DateUtilOSes+
                  VarutilsOses + ConvutilsOSes + ConvutilOSes + StdConvsOSes+
		  FmtBCDOSes + StrUtilsOSes;

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('rtl-objpas');
    P.ShortName:='rtlo';
    P.Directory:=ADirectory;
    P.Version:='3.1.1';
    P.Author := 'FPC core team';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.OSes:=AllTargetsObjPas;
    P.Email := '';
    P.Description := 'Rtl-objpas, aux. Delphi compat units';
    P.NeedLibC:= false;

    P.SourcePath.Add('src/inc');
    P.SourcePath.Add('src/$(OS)');
    P.SourcePath.Add('src/darwin',[iphonesim]);
    P.SourcePath.Add('src/unix',AllUnixOSes);
    P.SourcePath.Add('src/os2commn',[os2,emx]);
    P.SourcePath.Add('src/win',[win32,win64]);

    P.IncludePath.Add('src/inc');
    P.IncludePath.Add('src/unix',AllUnixOSes);
    P.IncludePath.Add('src/$(OS)');
    P.IncludePath.Add('src/darwin',[iphonesim]);


    T:=P.Targets.AddUnit('strutils.pp',StrUtilsOses);
    T:=P.Targets.AddUnit('varutils.pp',VarUtilsOses);
    with T.Dependencies do
      begin
        AddInclude('varutilh.inc');
        AddInclude('varutils.inc',VarUtilsOSes-[win32,win64]);
        AddInclude('cvarutil.inc');
      end;

    // 8.3
    T:=P.Targets.AddUnit('convutil.pp',ConvutilOSes);
    with T.Dependencies do
     begin
       AddInclude('convutil.inc');
     end;
 
    // normal
    T:=P.Targets.AddUnit('convutils.pp',ConvutilsOSes);
    with T.Dependencies do
     begin
       AddInclude('convutil.inc');
     end;

    // 8.3
    T:=P.Targets.AddUnit('dateutil.pp',dateutilOSes);
    with T.Dependencies do
     begin
       AddInclude('dateutil.inc');
     end;
 
    // normal
    T:=P.Targets.AddUnit('dateutils.pp',dateutilsOSes);
    with T.Dependencies do
     begin
       AddInclude('dateutil.inc');
     end;

    T:=P.Targets.AddUnit('stdconvs.pp',StdConvsOSes);
    T.ResourceStrings:=true;
    with T.Dependencies do
     begin
      AddUnit('convutils',ConvUtilsOSes);
      AddUnit('convutil',ConvUtilOSes);
     end;

    T:=P.Targets.AddUnit('fmtbcd.pp',FmtBCDOSes);
    with T.Dependencies do
      AddUnit('variants');

    T:=P.Targets.AddUnit('variants.pp',VariantsOSes);
    T.ResourceStrings:=true; 
    with T.Dependencies do
     begin
       AddUnit('varutils');
       // AddUnit('Math');
     end;
  end
end;
 
{$ifndef ALLPACKAGES}
begin
  add_rtl_objpas('');
  Installer.Run;
end.
{$endif ALLPACKAGES}

