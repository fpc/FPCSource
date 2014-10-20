{$ifndef ALLPACKAGES}
{$define allpackages}
{$define no_parent}
{$mode objfpc}{$H+}
program fpmake;

{$IFDEF MORPHOS}
 {$DEFINE NO_UNIT_PROCESS}
 {$DEFINE NO_THREADING}
{$ENDIF}

{$IFDEF OS2}
 {$DEFINE NO_UNIT_PROCESS}
{$ENDIF OS2}

{$IFDEF GO32V2}
 {$DEFINE NO_UNIT_PROCESS}
{$ENDIF GO32V2}

{$ifndef NO_UNIT_PROCESS}
  {$define HAS_UNIT_PROCESS}
{$endif NO_UNIT_PROCESS}

uses
  fpmkunit,
{$IFDEF HAS_UNIT_PROCESS}
  process,
{$ENDIF HAS_UNIT_PROCESS}
  sysutils;

{$endif ALLPACKAGES}

(*

The include files are generated with the following commands:

rm fpmake_proc.inc fpmake_add.inc ; /bin/ls -1 */fpmake.pp| while read file; do cleanedname=`dirname $file | sed -e 's+-+_+g'` ; if ! `grep -i "^procedure add_$cleanedname" $file >/dev/null` ; then printf 'procedure add_%s(const ADirectory: string);\nbegin\n  with Installer do\n{$include %s}\nend;\n\n' $cleanedname $file >> fpmake_proc.inc; else printf '{$include %s}\n\n' $file >> fpmake_proc.inc; fi; echo "  add_$cleanedname(ADirectory+IncludeTrailingPathDelimiter('$cleanedname'));" >> fpmake_add.inc; done

*)

{$include fpmake_proc.inc}

procedure add_utils(const ADirectory: string);

Var
  P : TPackage;
  T : TTarget;

begin
  With Installer do
    begin
    P:=AddPackage('utils');

    P.Author := '<various>';
    P.License := 'LGPL with modification';
    P.HomepageURL := 'www.freepascal.org';
    P.Email := '';
    P.Description := 'Various Free Pascal utilities.';
    P.NeedLibC:= false;
{$ifndef NO_PARENT}
    P.Directory:=ADirectory;
{$endif ALLPACKAGES}

    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('paszlib');
    P.Dependencies.Add('hash');
    P.Dependencies.Add('univint',[darwin,iphonesim]);

    P.Version:='2.7.1';

    T:=P.Targets.AddProgram('ptop.pp');
    T.Dependencies.AddUnit('ptopu');
    T.ResourceStrings:=true;

    P.Targets.AddProgram('ppdep.pp');
    P.Targets.AddProgram('rstconv.pp').ResourceStrings:=true;
    P.Targets.AddProgram('data2inc.pp');
    P.Targets.AddProgram('delp.pp');
    P.Targets.AddProgram('bin2obj.pp');
    P.Targets.AddProgram('postw32.pp');
    P.Targets.AddProgram('rmcvsdir.pp');
    P.Targets.AddProgram('grab_vcsa.pp',[linux]);
    T:=P.Targets.AddProgram('fpcsubst.pp');
    T.Dependencies.AddUnit('usubst');
    P.Targets.AddUnit('usubst.pp').install:=false;
    P.Targets.AddUnit('ptopu.pp').install:=false;
    end;

  {$include fpmake_add.inc}
end;

{$ifdef NO_PARENT}
begin
  add_utils('');

  Installer.Run;
end.
{$endif NO_PARENT}



