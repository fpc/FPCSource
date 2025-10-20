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

    P:=AddPackage('fcl-openapi');
    P.ShortName:='openapi';
{$ifdef ALLPACKAGES}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}
    P.Version:='3.3.1';
    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('rtl-objpas');
    P.Dependencies.Add('fcl-fpcunit');
    P.Dependencies.Add('fcl-json');
    P.Dependencies.Add('fcl-jsonschema');
    P.Dependencies.Add('regexpr');
    P.Author := 'Michael van Canneyt';
    P.License := 'LGPL with modification, ';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'OpenAPI (swagger) spec reader & code generator.';
    P.NeedLibC:= false;
    P.OSes:=AllOSes-[embedded,msdos,win16,macosclassic,palmos,zxspectrum,msxdos,amstradcpc,sinclairql,human68k,ps1,wasip2,oric];
    if Defaults.CPU=jvm then
      P.OSes := P.OSes - [java,android];

    P.SourcePath.Add('src');

    T:=P.Targets.AddUnit('fpopenapi.consts.pp');
//    T.ResourceStrings:=true;
    
    T:=P.Targets.AddUnit('fpopenapi.types.pp');
    with T.Dependencies do
      AddUnit('fpopenapi.consts');

    T:=P.Targets.AddUnit('fpopenapi.objects.pp');
    with T.Dependencies do
      begin
      AddUnit('fpopenapi.consts');
      AddUnit('fpopenapi.types');
      end;

    T:=P.Targets.AddUnit('fpopenapi.reader.pp');
    with T.Dependencies do
      begin
      AddUnit('fpopenapi.consts');
      AddUnit('fpopenapi.types');
      AddUnit('fpopenapi.objects');
      end;
    T:=P.Targets.AddUnit('fpopenapi.writer.pp');
    with T.Dependencies do
      begin
      AddUnit('fpopenapi.consts');
      AddUnit('fpopenapi.types');
      AddUnit('fpopenapi.objects');
      end;

   T:=P.Targets.AddUnit('fpopenapi.pascaltypes.pp');
   with T.Dependencies do
     begin
     AddUnit('fpopenapi.types');
     AddUnit('fpopenapi.objects');
     end;


   T:=P.Targets.AddUnit('fpopenapi.generators.pp');
   with T.Dependencies do
     begin
     AddUnit('fpopenapi.pascaltypes');
     AddUnit('fpopenapi.types');
     AddUnit('fpopenapi.objects');
     end;
   T:=P.Targets.AddUnit('fpopenapi.codegen.pp');
   with T.Dependencies do
     begin
     AddUnit('fpopenapi.pascaltypes');
     AddUnit('fpopenapi.types');
     AddUnit('fpopenapi.objects');
     AddUnit('fpopenapi.generators');
     end;


      
{$ifndef ALLPACKAGES}
    Run;
    end;
end.
{$endif ALLPACKAGES}



