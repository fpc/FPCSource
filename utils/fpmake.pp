{$mode objfpc}{$H+}
{$define allpackages}
program fpmake;

uses fpmkunit, sysutils;

procedure add_utils;

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

    P.Dependencies.Add('fcl-base');
    P.Dependencies.Add('paszlib');
    P.Dependencies.Add('hash');
    P.Dependencies.Add('univint',[darwin,iphonesim]);

    P.Version:='2.7.1';

    T:=P.Targets.AddProgram('ptop.pp');
    T.Dependencies.AddUnit('ptopu');
    T.ResourceStrings:=true;

    P.Targets.AddProgram('ppdep.pp');
    P.Targets.AddProgram('ptop.pp');
    P.Targets.AddProgram('rstconv.pp').ResourceStrings:=true;
    P.Targets.AddProgram('data2inc.pp');
    P.Targets.AddProgram('delp.pp');
    P.Targets.AddProgram('bin2obj.pp');
    P.Targets.AddProgram('postw32.pp');
    P.Targets.AddProgram('rmcvsdir.pp');
    P.Targets.AddProgram('grab_vcsa.pp');

    P.Targets.AddUnit('ptopu.pp').install:=false;
    end;
end;

(*

The include files are generated with the following commands:

rm fpmake_proc.inc fpmake_add.inc ; /bin/ls -1 */fpmake.pp| while read file; do cleanedname=`dirname $file | sed -e 's+-+_+g'` ; if ! `grep -i "^procedure add_$cleanedname" $file >/dev/null` ; then printf 'procedure add_%s;\nbegin\n  with Installer do\n{$include %s}\nend;\n\n' $cleanedname $file >> fpmake_proc.inc; else printf '{$include %s}\n\n' $file >> fpmake_proc.inc; fi; echo "  add_$cleanedname;" >> fpmake_add.inc; done

*)

{$include fpmake_proc.inc}

begin
{$include fpmake_add.inc}
  add_utils;

  Installer.Run;
end.




