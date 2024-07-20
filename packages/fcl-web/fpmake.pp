{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;

{$endif ALLPACKAGES}

procedure add_fcl_web(const ADirectory: string);

Const
  LibMicroHttpdOSes = AllUnixOSes + [win32,win64];
  SqldbConnectionOSes = [aix,beos,haiku,linux,freebsd,darwin,iphonesim,ios,netbsd,openbsd,solaris,win32,win64,wince,android,dragonfly];
  SqliteOSes          = [aix,beos,haiku,linux,freebsd,darwin,iphonesim,ios,netbsd,openbsd,solaris,win32,win64,wince,android,dragonfly];

Var
  T : TTarget;
  P : TPackage;
begin
  With Installer do
    begin
    P:=AddPackage('fcl-web');
    P.ShortName:='fclw';
    P.Directory:=ADirectory;
    P.Version:='3.2.4-rc1';
    P.OSes := [beos,haiku,freebsd,darwin,iphonesim,ios,solaris,netbsd,openbsd,linux,win32,win64,wince,aix,amiga,aros,morphos,dragonfly,android];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('fcl-db');
    P.Dependencies.Add('fcl-xml');
    P.Dependencies.Add('fcl-json');
    P.Dependencies.Add('fcl-net');
    P.Dependencies.Add('fcl-process');
    P.Dependencies.Add('fcl-fpcunit');
    P.Dependencies.Add('fcl-hash');
    P.Dependencies.Add('hash');
    P.Dependencies.Add('fcl-registry',AllWindowsOSes);
    P.Dependencies.Add('openssl',AllUnixOSes+AllWindowsOSes);
    P.Dependencies.Add('fastcgi');
    P.Dependencies.Add('httpd22', AllOses - [amiga,aros,morphos]);
    P.Dependencies.Add('httpd24', AllOses - [amiga,aros,morphos]);
    P.Dependencies.Add('winunits-base', [Win32,Win64]);
    // (Temporary) indirect dependencies, not detected by fpcmake:
    P.Dependencies.Add('univint',[MacOSX,iphonesim,ios]);
    P.Dependencies.Add('libmicrohttpd',LibMicroHttpdOSes);
    P.Author := 'FreePascal development team';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Web(app) related parts of Free Component Libraries (FCL), FPC''s OOP library.';
    P.NeedLibC:= false;

    P.SourcePath.Add('src/base');
    P.SourcePath.Add('src/webdata');
    P.SourcePath.Add('src/jwt');
    P.SourcePath.Add('src/jsonrpc');
    P.SourcePath.Add('src/hpack');
    P.SourcePath.Add('src/restbridge');
    T:=P.Targets.addUnit('fpmimetypes.pp');

    T:=P.Targets.AddUnit('httpdefs.pp');
    T.ResourceStrings:=true;
    T.Dependencies.AddUnit('httpprotocol');

    T:=P.Targets.AddUnit('httproute.pp');
      T.ResourceStrings:=true;
    T.Dependencies.AddUnit('httpdefs');

    T:=P.Targets.AddUnit('cgiapp.pp');
    T.ResourceStrings:=true;
    T:=P.Targets.AddUnit('custcgi.pp');
    T.ResourceStrings:=true;
      with T.Dependencies do
        begin
          AddUnit('httpprotocol');
          AddUnit('cgiprotocol');
          AddUnit('httpdefs');
        end;
    T:=P.Targets.AddUnit('ezcgi.pp');
    T:=P.Targets.AddUnit('fpcgi.pp');
      with T.Dependencies do
        begin
          AddUnit('httpdefs');
          AddUnit('fphttp');
        end;
    T:=P.Targets.AddUnit('fpdatasetform.pp');
      with T.Dependencies do
        begin
          AddUnit('fphtml');
        end;
    T:=P.Targets.AddUnit('fphtml.pp');
    T.ResourceStrings:=true;
      with T.Dependencies do
        begin
          AddUnit('httpdefs');
          AddUnit('fphttp');
        end;
    T:=P.Targets.AddUnit('fphttp.pp');
    T.ResourceStrings:=true;
      with T.Dependencies do
        begin
          AddUnit('httpdefs');
        end;
    T:=P.Targets.AddUnit('fpweb.pp');
    T.ResourceStrings:=true;
      with T.Dependencies do
        begin
          AddUnit('httpdefs');
          AddUnit('fphttp');
          AddUnit('websession');
        end;
    T:=P.Targets.AddUnit('httpprotocol.pp');
    T:=P.Targets.AddUnit('cgiprotocol.pp');

    
    T:=P.Targets.AddUnit('iniwebsession.pp');
    T.ResourceStrings:=true;
      with T.Dependencies do
        begin
          AddUnit('fphttp');
          AddUnit('httpdefs');
        end;
    T:=P.Targets.AddUnit('websession.pp');
      with T.Dependencies do
        begin
          AddUnit('iniwebsession');
        end;
    T:=P.Targets.AddUnit('webutil.pp');
      with T.Dependencies do
        begin
          AddUnit('httpdefs');
        end;
    with P.Targets.AddUnit('custweb.pp') do
      begin
        ResourceStrings:=true;
        Dependencies.AddUnit('httpdefs');
        Dependencies.AddUnit('httproute');
        Dependencies.AddUnit('fphttp');
      end;
    with P.Targets.AddUnit('webpage.pp') do
      begin
        Dependencies.AddUnit('httpdefs');
        Dependencies.AddUnit('fphtml');
        Dependencies.AddUnit('fpweb');
      end;
    with P.Targets.AddUnit('fpfcgi.pp') do
      begin
        OSes:=AllOses-[wince,darwin,iphonesim,ios,aix,amiga,aros,morphos];
        Dependencies.AddUnit('custfcgi');
      end;
    with P.Targets.AddUnit('custfcgi.pp') do
      begin
        OSes:=AllOses-[wince,darwin,iphonesim,ios,aix,amiga,aros,morphos];
        Dependencies.AddUnit('httpprotocol');
        Dependencies.AddUnit('cgiprotocol');
        Dependencies.AddUnit('custcgi');
        Dependencies.AddUnit('httpdefs');
        Dependencies.AddUnit('custweb');
        ResourceStrings:=true;
      end;
    with P.Targets.AddUnit('custapache.pp') do
      begin
        OSes:=AllOses-[amiga,aros,morphos];
        Dependencies.AddUnit('httpprotocol');
        Dependencies.AddUnit('fphttp');
        Dependencies.AddUnit('custweb');
        ResourceStrings:=true;
      end;
    with P.Targets.AddUnit('fpapache.pp') do
      begin
        OSes:=AllOses-[amiga,aros,morphos];
        Dependencies.AddUnit('custapache');
      end;
    with P.Targets.AddUnit('custapache24.pp') do
      begin
        OSes:=AllOses-[amiga,aros,morphos];
        Dependencies.AddUnit('fphttp');
        Dependencies.AddUnit('custweb');
        ResourceStrings:=true;
      end;
    with P.Targets.AddUnit('fpapache24.pp') do
      begin
        OSes:=AllOses-[amiga,aros,morphos];
        Dependencies.AddUnit('custapache24');
      end;
    with P.Targets.AddUnit('custhttpsys.pp') do
      begin
        OSes:=[Win32,Win64];
        Dependencies.AddUnit('custweb');
        Dependencies.AddUnit('httpdefs');
        Dependencies.AddUnit('httpprotocol');
        ResourceStrings:=true;
      end;
    with P.Targets.AddUnit('fphttpsys.pp') do
      begin
        OSes:=[Win32,Win64];
        Dependencies.AddUnit('custhttpsys');
      end;
    with P.Targets.AddUnit('custmicrohttpapp.pp') do
      begin
        Dependencies.AddUnit('custweb');
        Dependencies.AddUnit('httpdefs');
        Dependencies.AddUnit('httpprotocol');
        ResourceStrings:=true;
        OSes := LibMicroHttpdOSes;
        if Defaults.CPU=jvm then
          OSes := OSes - [java,android];
      end;  
    with P.Targets.AddUnit('microhttpapp.pp') do
      begin
        Dependencies.AddUnit('custweb');
        Dependencies.AddUnit('httpdefs');
        Dependencies.AddUnit('httpprotocol');
        Dependencies.AddUnit('custmicrohttpapp');
        OSes := LibMicroHttpdOSes;
        if Defaults.CPU=jvm then
          OSes := OSes - [java,android];
      end;  

      
    with P.Targets.AddUnit('fphttpstatus.pas') do
      begin
        Dependencies.AddUnit('fphttpserver');
        Dependencies.AddUnit('HTTPDefs');
      end;
    T:=P.Targets.AddUnit('fcgigate.pp');
    T.ResourceStrings:=true;
    With T.Dependencies do
      begin
      AddUnit('httpdefs');
      AddUnit('custcgi');
      end;
    T:=P.Targets.AddUnit('fphttpserver.pp');
    T.ResourceStrings:=true;
      with T.Dependencies do
        begin
          AddUnit('httpdefs');
        end;
    T:=P.Targets.AddUnit('fphttpclient.pp');
    T.ResourceStrings:=true;
    T:=P.Targets.AddUnit('custhttpapp.pp');
    // T.ResourceStrings:=true;
    T:=P.Targets.AddUnit('fphttpapp.pp');
    T:=P.Targets.AddUnit('fpwebfile.pp');
    With T.Dependencies do
      begin
      AddUnit('fphttp');
      AddUnit('httpdefs');
      AddUnit('httproute');
      end;
    T:=P.Targets.AddUnit('fpwebproxy.pp');
    With T.Dependencies do
      begin
      AddUnit('fphttp');
      AddUnit('httpdefs');
      AddUnit('httpprotocol');
      AddUnit('fphttpclient');
      end;
    T.ResourceStrings:=true;
    T:=P.Targets.AddUnit('fpwebdata.pp');
    T.ResourceStrings:=true;
    With T.Dependencies do
      begin
      AddUnit('httpdefs');
      AddUnit('fphttp');
      AddUnit('websession');
      end;
    T:=P.Targets.AddUnit('sqldbwebdata.pp');
    T.ResourceStrings:=true;
    T.OSes:=SqldbConnectionOSes;
    With T.Dependencies do
      begin
      AddUnit('fpwebdata');
      AddUnit('fphttp');
      end;
    T:=P.Targets.AddUnit('fpextjs.pp');
    T.ResourceStrings:=true;
    With T.Dependencies do
      begin
      AddUnit('fpwebdata');
      AddUnit('httpdefs');
      AddUnit('fphttp');
      end;
    T:=P.Targets.AddUnit('extjsxml.pp');
    T.ResourceStrings:=true;
    With T.Dependencies do
      begin
      AddUnit('fpwebdata');
      AddUnit('httpdefs');
      AddUnit('fpextjs');
      end;
    T:=P.Targets.AddUnit('extjsjson.pp');
    T.ResourceStrings:=true;
    With T.Dependencies do
      begin
      AddUnit('fpwebdata');
      AddUnit('httpdefs');
      AddUnit('fpextjs');
      end;
    T:=P.Targets.AddUnit('fpjsonrpc.pp');
    T.ResourceStrings:=true;
    T:=P.Targets.AddUnit('webjsonrpc.pp');
    With T.Dependencies do
      begin
      AddUnit('fpjsonrpc');
      end;
    T:=P.Targets.AddUnit('fpdispextdirect.pp');
    With T.Dependencies do
      begin
      AddUnit('fpjsonrpc');
      end;
    T:=P.Targets.AddUnit('fpextdirect.pp');
    T.ResourceStrings:=true;
    With T.Dependencies do
      begin
      AddUnit('fpdispextdirect');
      AddUnit('webjsonrpc');
      AddUnit('httpdefs');
      end;
    T:=P.Targets.AddUnit('fpwebclient.pp');
    T:=P.Targets.AddUnit('fpjwt.pp');
    T:=P.Targets.AddUnit('fpoauth2.pp');
      T.ResourceStrings:=true;
    T.Dependencies.AddUnit('fpwebclient');
    T.Dependencies.AddUnit('fpjwt');
    T:=P.Targets.AddUnit('fpoauth2ini.pp');
    T.Dependencies.AddUnit('fpoauth2');
    T:=P.Targets.AddUnit('fpjwasha256.pp');
    T.Dependencies.AddUnit('fpjwt');
    T:=P.Targets.AddUnit('fpjwasha512.pp');
    T.Dependencies.AddUnit('fpjwt');
    T:=P.Targets.AddUnit('fpjwasha384.pp');
    T.Dependencies.AddUnit('fpjwt');
    T:=P.Targets.AddUnit('fpjwaes256.pp');
    T.Dependencies.AddUnit('fpjwt');
    T:=P.Targets.AddUnit('fphttpwebclient.pp');
    T.Dependencies.AddUnit('fpwebclient');
    T:=P.Targets.AddUnit('restbase.pp');
    T:=P.Targets.AddUnit('restcodegen.pp');

    T:=P.Targets.AddUnit('uhpacktables.pp');
    T:=P.Targets.AddUnit('uhpackimp.pp');
    With T.Dependencies do  
      AddUnit('uhpacktables');
    T:=P.Targets.AddUnit('uhpack.pp');
    With T.Dependencies do  
      begin
      AddUnit('uhpackimp');
      end;
    
    T:=P.Targets.AddUnit('sqldbrestconst.pp');
    T.ResourceStrings:=true;
    T.OSes:=SqldbConnectionOSes;
    
    T:=P.Targets.AddUnit('sqldbrestschema.pp');
    T.OSes:=SqldbConnectionOSes;
    With T.Dependencies do  
      begin
      AddUnit('sqldbrestconst');
      end;
    T:=P.Targets.AddUnit('sqldbrestio.pp');
    T.OSes:=SqldbConnectionOSes;
    With T.Dependencies do  
      begin
      AddUnit('sqldbrestconst');
      AddUnit('sqldbrestschema');
      end;
    T:=P.Targets.AddUnit('sqldbrestdata.pp');
    T.OSes:=SqldbConnectionOSes;
    With T.Dependencies do  
      begin
      AddUnit('sqldbrestconst');
      AddUnit('sqldbrestschema');
      AddUnit('sqldbrestio');
      end;
    T:=P.Targets.AddUnit('sqldbrestauth.pp');
    T.OSes:=SqldbConnectionOSes;
    With T.Dependencies do  
      begin
      AddUnit('sqldbrestconst');
      AddUnit('sqldbrestio');
      AddUnit('sqldbrestschema');
      end;
    T:=P.Targets.AddUnit('sqldbrestjson.pp');
    T.OSes:=SqldbConnectionOSes;
    With T.Dependencies do  
      begin
      AddUnit('sqldbrestio');
      AddUnit('sqldbrestschema');
      AddUnit('sqldbrestconst');
      end;
    T:=P.Targets.AddUnit('sqldbrestbridge.pp');
    T.OSes:=SqldbConnectionOSes;
    With T.Dependencies do  
      begin
      AddUnit('sqldbrestio');
      AddUnit('sqldbrestdata');
      AddUnit('sqldbrestschema');
      AddUnit('sqldbrestconst');
      end;
    T:=P.Targets.AddUnit('sqldbrestcds.pp');
    T.OSes:=SqldbConnectionOSes;
    With T.Dependencies do  
      begin
      AddUnit('sqldbrestio');
      AddUnit('sqldbrestschema');
      AddUnit('sqldbrestconst');
      end;
    T:=P.Targets.AddUnit('sqldbrestcsv.pp');
    T.OSes:=SqldbConnectionOSes;
    With T.Dependencies do  
      begin
      AddUnit('sqldbrestio');
      AddUnit('sqldbrestschema');
      AddUnit('sqldbrestconst');
      end;
    T:=P.Targets.AddUnit('sqldbrestxml.pp');
    T.OSes:=SqldbConnectionOSes;
    With T.Dependencies do  
      begin
      AddUnit('sqldbrestio');
      AddUnit('sqldbrestschema');
      AddUnit('sqldbrestconst');
      end;
    T:=P.Targets.AddUnit('sqldbrestado.pp');
    T.OSes:=SqldbConnectionOSes;
    With T.Dependencies do  
      begin
      AddUnit('sqldbrestio');
      AddUnit('sqldbrestschema');
      AddUnit('sqldbrestconst');
      end;
    T:=P.Targets.AddUnit('sqldbrestini.pp');
    T.OSes:=SqldbConnectionOSes;
    With T.Dependencies do  
      begin
      AddUnit('sqldbrestbridge');
      AddUnit('sqldbrestschema');
      AddUnit('sqldbrestconst');
      end;
    T:=P.Targets.AddUnit('sqldbrestauthini.pp');
    T.OSes:=SqldbConnectionOSes;
    With T.Dependencies do  
      begin
      AddUnit('sqldbrestauth');
      AddUnit('sqldbrestschema');
      AddUnit('sqldbrestconst');
      end;
    T:=P.Targets.AddUnit('sqldbrestmodule.pp');
    T.OSes:=SqldbConnectionOSes;
    With T.Dependencies do  
      begin
      AddUnit('sqldbrestbridge');
      AddUnit('sqldbrestconst');
      end;
    end;
end;
    
{$ifndef ALLPACKAGES}
begin
  add_fcl_web('');
  Installer.Run;
end.
{$endif ALLPACKAGES}
