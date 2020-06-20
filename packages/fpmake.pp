{$ifndef ALLPACKAGES}
{$define allpackages}
{$define no_parent}
{$mode objfpc}{$H+}
program fpmake;

uses {$ifdef unix}cthreads,{$endif} sysutils, Classes, fpmkunit;

Var
  TBuild,T : TTarget;
  PBuild,P : TPackage;
  D : TDependency;
  I : Integer;

{$endif ALLPACKAGES}

(*

The include files are generated with the following commands:

rm fpmake_proc.inc fpmake_add.inc ; /bin/ls -1 */fpmake.pp| while read file; do dir=`dirname $file` ; cleanedname=`echo $dir | sed -e 's+-+_+g'` ; if ! `grep -i "^procedure add_$cleanedname" $file >/dev/null` ; then printf 'procedure add_%s(const ADirectory: string);\nbegin\n  with Installer do\n{$include %s}\nend;\n\n' $cleanedname $file >> fpmake_proc.inc; else printf '{$include %s}\n\n' $file >> fpmake_proc.inc; fi; echo "  add_$cleanedname(ADirectory+IncludeTrailingPathDelimiter('$dir'));" >> fpmake_add.inc; done

*)

{$include fpmake_proc.inc}

procedure add_packages_comandlineoptions();
begin
  AddCustomFpmakeCommandlineOption('data2inc', 'Use indicated data2inc executable.');
  AddCustomFpmakeCommandlineOption('genfpmkunit', 'Regenerate the fpmkunitsrc.inc file (fppkg).');
  add_ide_comandlineoptions();
end;

procedure add_packages(const ADirectory: string);

begin

{$include fpmake_add.inc}

  With Installer do
    begin
      // Create fpc-all package
      PBuild:=AddPackage('fpc-all');
      PBuild.Version:='3.2.1';
    end;
end;

{$ifdef no_parent}
begin
  add_packages_comandlineoptions();
  add_packages('');
  Installer.Run;
end.
{$endif no_parent}
