{$ifndef ALLPACKAGES}
{$mode objfpc}{$H+}
program fpmake;

uses fpmkunit;
{$endif ALLPACKAGES}

procedure add_fcl_sonar(const ADirectory: string);
var
  P : TPackage;
  T : TTarget;

begin
  with Installer do
    begin
      P := AddPackage('fcl-sonar');
      P.Version := '3.3.1';
      P.Directory := ADirectory;
      P.Author := 'Michael Van Canneyt';
      P.License := 'modified LGPL';
      P.Description := 'Object Pascal static analyzer (linter)';

      P.OSes:=AllOSes-[embedded,msdos,win16,go32v2,nativent,macosclassic,palmos,atari,zxspectrum,msxdos,amstradcpc,sinclairql,wasip1,wasip1threads,human68k,ps1,wasip2];
      if Defaults.CPU=jvm then
        P.OSes := P.OSes - [java,android];

      P.Dependencies.Add('fcl-passrc');
      P.Dependencies.Add('fcl-fpcunit');
      P.Dependencies.Add('fcl-xml');
      P.Dependencies.Add('fcl-json');
      P.Dependencies.Add('fcl-process');
      P.Dependencies.Add('regexpr');
      P.SourcePath.Add('src/base');
      P.SourcePath.Add('src/rules');
      P.SourcePath.Add('src/output');

      P.Targets.AddUnit('fpsonar.consts.pp');

      T := P.Targets.AddUnit('fpsonar.types.pp');
      T.Dependencies.AddUnit('fpsonar.consts');

      T := P.Targets.AddUnit('fpsonar.config.pp');
      T.Dependencies.AddUnit('fpsonar.types');

      T := P.Targets.AddUnit('fpsonar.baseline.pp');
      T.Dependencies.AddUnit('fpsonar.types');

      T := P.Targets.AddUnit('fpsonar.fpcsource.pp');
      T.Dependencies.AddUnit('fpsonar.types');

      T := P.Targets.AddUnit('fpsonar.ingest.pp');
      T.Dependencies.AddUnit('fpsonar.types');

      P.Targets.AddUnit('fpsonar.ppustub.pp');

      T := P.Targets.AddUnit('fpsonar.issues.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.config');
      T.Dependencies.AddUnit('fpsonar.baseline');
      T.Dependencies.AddUnit('fpsonar.ingest');

      T := P.Targets.AddUnit('fpsonar.resolver.pp');
      T.Dependencies.AddUnit('fpsonar.ingest');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.ppustub');
      T.Dependencies.AddUnit('fpsonar.consts');

      T := P.Targets.AddUnit('fpsonar.traversal.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.resolver');

      T := P.Targets.AddUnit('fpsonar.sourcefile.pp');
      T.Dependencies.AddUnit('fpsonar.ingest');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.resolver');

      T := P.Targets.AddUnit('fpsonar.ruleframework.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.config');
      T.Dependencies.AddUnit('fpsonar.issues');
      T.Dependencies.AddUnit('fpsonar.ingest');
      T.Dependencies.AddUnit('fpsonar.resolver');
      T.Dependencies.AddUnit('fpsonar.traversal');
      T.Dependencies.AddUnit('fpsonar.SourceFile');
      T.Dependencies.AddUnit('fpsonar.consts');

      T := P.Targets.AddUnit('fpsonar.engine.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.ruleframework');
      T.Dependencies.AddUnit('fpsonar.issues');
      T.Dependencies.AddUnit('fpsonar.config');
      T.Dependencies.AddUnit('fpsonar.SourceFile');
      T.Dependencies.AddUnit('fpsonar.traversal');
      T.Dependencies.AddUnit('fpsonar.fpcsource');

      // --- src/output ---
      T := P.Targets.AddUnit('fpsonar.output.utils.pp');
      T.Dependencies.AddUnit('fpsonar.types');

      T := P.Targets.AddUnit('fpsonar.output.text.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.output.utils');

      T := P.Targets.AddUnit('fpsonar.output.sarif.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.output.utils');

      T := P.Targets.AddUnit('fpsonar.output.sonarjson.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.output.utils');

      // --- src/rules ---
      P.Targets.AddUnit('fpsonar.rules.consts.pp');

      T := P.Targets.AddUnit('fpsonar.rules.calls.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.issues');
      T.Dependencies.AddUnit('fpsonar.ruleframework');
      T.Dependencies.AddUnit('fpsonar.traversal');
      T.Dependencies.AddUnit('fpsonar.resolver');
      T.Dependencies.AddUnit('fpsonar.rules.consts');

      T := P.Targets.AddUnit('fpsonar.rules.casts.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.issues');
      T.Dependencies.AddUnit('fpsonar.ruleframework');
      T.Dependencies.AddUnit('fpsonar.traversal');
      T.Dependencies.AddUnit('fpsonar.resolver');
      T.Dependencies.AddUnit('fpsonar.rules.consts');

      T := P.Targets.AddUnit('fpsonar.rules.classes.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.issues');
      T.Dependencies.AddUnit('fpsonar.ruleframework');
      T.Dependencies.AddUnit('fpsonar.traversal');
      T.Dependencies.AddUnit('fpsonar.config');
      T.Dependencies.AddUnit('fpsonar.rules.consts');

      T := P.Targets.AddUnit('fpsonar.rules.control.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.issues');
      T.Dependencies.AddUnit('fpsonar.ruleframework');
      T.Dependencies.AddUnit('fpsonar.traversal');
      T.Dependencies.AddUnit('fpsonar.resolver');
      T.Dependencies.AddUnit('fpsonar.rules.consts');

      T := P.Targets.AddUnit('fpsonar.rules.exceptions.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.issues');
      T.Dependencies.AddUnit('fpsonar.ruleframework');
      T.Dependencies.AddUnit('fpsonar.traversal');
      T.Dependencies.AddUnit('fpsonar.rules.consts');

      T := P.Targets.AddUnit('fpsonar.rules.forms.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.issues');
      T.Dependencies.AddUnit('fpsonar.ruleframework');
      T.Dependencies.AddUnit('fpsonar.traversal');
      T.Dependencies.AddUnit('fpsonar.config');
      T.Dependencies.AddUnit('fpsonar.rules.consts');

      T := P.Targets.AddUnit('fpsonar.rules.imports.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.issues');
      T.Dependencies.AddUnit('fpsonar.ruleframework');
      T.Dependencies.AddUnit('fpsonar.resolver');
      T.Dependencies.AddUnit('fpsonar.rules.consts');

      T := P.Targets.AddUnit('fpsonar.rules.layout.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.issues');
      T.Dependencies.AddUnit('fpsonar.ruleframework');
      T.Dependencies.AddUnit('fpsonar.ingest');
      T.Dependencies.AddUnit('fpsonar.config');
      T.Dependencies.AddUnit('fpsonar.rules.consts');

      T := P.Targets.AddUnit('fpsonar.rules.naming.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.issues');
      T.Dependencies.AddUnit('fpsonar.ruleframework');
      T.Dependencies.AddUnit('fpsonar.traversal');
      T.Dependencies.AddUnit('fpsonar.config');
      T.Dependencies.AddUnit('fpsonar.rules.consts');

      T := P.Targets.AddUnit('fpsonar.rules.parens.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.issues');
      T.Dependencies.AddUnit('fpsonar.ruleframework');
      T.Dependencies.AddUnit('fpsonar.ingest');
      T.Dependencies.AddUnit('fpsonar.rules.consts');

      T := P.Targets.AddUnit('fpsonar.rules.refs.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.issues');
      T.Dependencies.AddUnit('fpsonar.ruleframework');
      T.Dependencies.AddUnit('fpsonar.traversal');
      T.Dependencies.AddUnit('fpsonar.resolver');
      T.Dependencies.AddUnit('fpsonar.rules.consts');

      T := P.Targets.AddUnit('fpsonar.rules.semnaming.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.issues');
      T.Dependencies.AddUnit('fpsonar.ruleframework');
      T.Dependencies.AddUnit('fpsonar.traversal');
      T.Dependencies.AddUnit('fpsonar.resolver');
      T.Dependencies.AddUnit('fpsonar.rules.consts');

      T := P.Targets.AddUnit('fpsonar.rules.structure.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.issues');
      T.Dependencies.AddUnit('fpsonar.ruleframework');
      T.Dependencies.AddUnit('fpsonar.traversal');
      T.Dependencies.AddUnit('fpsonar.ingest');
      T.Dependencies.AddUnit('fpsonar.config');
      T.Dependencies.AddUnit('fpsonar.rules.consts');

      T := P.Targets.AddUnit('fpsonar.rules.tokens.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.issues');
      T.Dependencies.AddUnit('fpsonar.ruleframework');
      T.Dependencies.AddUnit('fpsonar.ingest');
      T.Dependencies.AddUnit('fpsonar.rules.consts');

      T := P.Targets.AddUnit('fpsonar.rules.trackers.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.issues');
      T.Dependencies.AddUnit('fpsonar.ruleframework');
      T.Dependencies.AddUnit('fpsonar.config');
      T.Dependencies.AddUnit('fpsonar.resolver');
      T.Dependencies.AddUnit('fpsonar.traversal');
      T.Dependencies.AddUnit('fpsonar.rules.consts');

      T := P.Targets.AddUnit('fpsonar.rules.unused.pp');
      T.Dependencies.AddUnit('fpsonar.types');
      T.Dependencies.AddUnit('fpsonar.issues');
      T.Dependencies.AddUnit('fpsonar.ruleframework');
      T.Dependencies.AddUnit('fpsonar.traversal');
      T.Dependencies.AddUnit('fpsonar.config');
      T.Dependencies.AddUnit('fpsonar.rules.consts');
    end;
end;

{$ifndef ALLPACKAGES}
begin
  add_fcl_sonar('');
  Installer.Run;
end.
{$endif ALLPACKAGES}

