program fpmake;

{$mode objfpc}{$H+}

uses
  fpmakeunit
  { add your units here };

begin
  With Installer Do
    begin
    StartPackage('Mypackage');
    Version:='1.0';
    URL:='http://www.freepascal.org/';
    Targets.AddUnit('myunit');
    Targets['myunit'].OS:=[Win32,Linux];
    Targets['myunit'].Resourcestrings:=True;
    Targets.AddUnit('testbuild/myotherunit').OS:=[Linux];
    Targets.AddProgram('myprog');
    EndPackage;
    Run;
    end;
end.

