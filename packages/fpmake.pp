{$mode objfpc}{$H+}
{$define allpackages}
program fpmake;

uses fpmkunit;

Var
  TBuild,T : TTarget;
  PBuild,P : TPackage;
  D : TDependency;
  I : Integer;

(*

The include files are generated with the following commands:

/bin/ls -1 */fpmake.pp | awk -F '/' '/fpmake.pp/ { printf "procedure add_%s;\nbegin\n  with Installer do\n{$include %s}\nend;\n\n",gensub("-","_","g",$1),$0; }' > fpmake_proc.inc
/bin/ls -1 */fpmake.pp | awk -F '/' '/fpmake.pp/ { printf "  add_%s;\n",gensub("-","_","g",$1); }' > fpmake_add.inc

*)

{$include fpmake_proc.inc}

begin
{$include fpmake_add.inc}

  With Installer do
    begin
      // Create fpc-all package
      PBuild:=AddPackage('fpc-all');
      PBuild.Version:='2.2.2-0';
      for i:=0 to Packages.Count-1 do
        begin
          P:=Packages.PackageItems[i];
          if P.Name<>'fpc-all' then
            D:=PBuild.Dependencies.Add(P.Name);
        end;

      Run;
    end;
end.
