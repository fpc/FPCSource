{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} fpmkunit;

Var
  T : TTarget;
  P : TPackage;
begin
  With Installer do
    begin
{$endif ALLPACKAGES}

    P:=AddPackage('fcl-jsonschema');
    P.ShortName:='fcljschm';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('rtl-objpas');
    P.Dependencies.Add('fcl-fpcunit');
    P.Dependencies.Add('fcl-json');
    P.Dependencies.Add('regexpr');
    P.Author := 'Michael van Canneyt';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Json Schema validator implementation.';
    P.NeedLibC:= false;
    P.OSes:=AllOSes-[embedded,msdos,win16,macosclassic,palmos,zxspectrum,msxdos,amstradcpc,sinclairql,human68k];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('fpjson.schema.consts.pp');
    T.ResourceStrings:=true;
    
    T:=P.Targets.AddUnit('fpjson.schema.types.pp');
    with T.Dependencies do
      AddUnit('fpjson.schema.consts');

    T:=P.Targets.AddUnit('fpjson.schema.schema.pp');
    with T.Dependencies do
      begin
      AddUnit('fpjson.schema.consts');
      AddUnit('fpjson.schema.types');
      end;

    T:=P.Targets.AddUnit('fpjson.schema.reader.pp');
    with T.Dependencies do
      begin
      AddUnit('fpjson.schema.consts');
      AddUnit('fpjson.schema.types');
      AddUnit('fpjson.schema.schema');
      end;
    T:=P.Targets.AddUnit('fpjson.schema.loader.pp');
    with T.Dependencies do
      begin
      AddUnit('fpjson.schema.consts');
      AddUnit('fpjson.schema.types');
      AddUnit('fpjson.schema.schema');
      end;
    T:=P.Targets.AddUnit('fpjson.schema.writer.pp');
    with T.Dependencies do
      begin
      AddUnit('fpjson.schema.consts');
      AddUnit('fpjson.schema.types');
      AddUnit('fpjson.schema.schema');
      end;

    T:=P.Targets.AddUnit('fpjson.schema.validator.pp');
    with T.Dependencies do
      begin
      AddUnit('fpjson.schema.consts');
      AddUnit('fpjson.schema.types');
      AddUnit('fpjson.schema.schema');
      end;
      
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}



