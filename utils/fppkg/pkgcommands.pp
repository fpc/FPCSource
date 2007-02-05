unit pkgcommands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,pkghandler;

type
  { TCommandUpdate }

  TCommandUpdate = Class(TPackagehandler)
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;
  
  { TCommandDownload }

  TCommandDownload = Class(TPackagehandler)
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;

  { TCommandBuild }

  TCommandBuild = Class(TPackagehandler)
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;


  { TCommandInstall }

  TCommandInstall = Class(TPackagehandler)
  Public
    Function Execute(const Args:TActionArgs):boolean;override;
  end;


implementation

uses
  pkgmessages,
  fpmktype,
  fprepos,
  fpxmlrep;
  
function TCommandUpdate.Execute(const Args:TActionArgs):boolean;
Var
  X : TFPXMLRepositoryHandler;
  P : TFPPackage;
  R : TFPRepository;
begin
{$warning TODO remove this hack}
  R:=TFPRepository.Create(nil);
  P:=R.AddPackage('FirstPackage');
  P.Author:='Michael Van Canneyt';
  P.URL:='http://www.freepascal.org/packages/firstpackage.zip';
  P.Email:='michael@freepascal.org';
  P.Version.AsString:='1.2.3';
  P.Description:='First package in the repository. Provides basic information.';
  P.OSes:=[Win32,linux];
  P.CPUs:=[i386,x86_64];
  X:=TFPXMLRepositoryHandler.Create;
  With X do
    try
      SaveToXml(R,Defaults.LocalRepository);
    finally
      Free;
    end;
  FreeAndNil(R);
end;


function TCommandDownload.Execute(const Args:TActionArgs):boolean;
begin
  ActionStack.Push(CurrentPackage,'downloadpackage',Args);
end;


function TCommandBuild.Execute(const Args:TActionArgs):boolean;
begin
  ActionStack.Push(CurrentPackage,'fpmakebuild',Args);
  ActionStack.Push(CurrentPackage,'compilefpmake',Args);
end;


function TCommandInstall.Execute(const Args:TActionArgs):boolean;
begin
  ActionStack.Push(CurrentPackage,'fpmakeinstall',Args);
  ActionStack.Push(CurrentPackage,'build',Args);
end;


initialization
  RegisterPkgHandler('update',TCommandUpdate);
  RegisterPkgHandler('download',TCommandDownload);
  RegisterPkgHandler('build',TCommandBuild);
  RegisterPkgHandler('install',TCommandInstall);
end.
