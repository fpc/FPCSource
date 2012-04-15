{$ifdef Delphi}
const fpmake : array[0..2] of string[240]=(
{$else Delphi}
const fpmake : array[0..2,1..240] of char=(
{$endif Delphi}
  '{$ifndef ALLPACKAGES}'#010+
  '{$mode objfpc}{$H+}'#010+
  'program fpmake;'#010+
  #010+
  'uses fpmkunit;'#010+
  #010+
  'Var'#010+
  '  T : TTarget;'#010+
  '  P : TPackage;'#010+
  'begin'#010+
  '  With Installer do'#010+
  '    begin'#010+
  '{$endif ALLPACKAGES}'#010+
  '    P:=AddPackage('#039'%packagename%'#039');'#010+
  '{$ifdef ALLPACKAGES}'#010+
  '    P.Directory:','='#039'%directory%'#039';'#010+
  '{$endif ALLPACKAGES}'#010+
  '    P.Version:='#039'%version%'#039';'#010+
  '    P.License:='#039'%license%'#039';'#010+
  '    P.Author:='#039'%author%'#039';'#010+
  '    P.Email:='#039'%email%'#039';'#010+
  '    P.Description:=%quotedstr(description)%;'#010+
  '    P.HomepageURL:='#039'%homepageurl%'#039';'#010+
  '%conditionalpack','ageproperties%'#010+
  '%packagedependencies%'#010+
  '%packagesourcepaths%'#010+
  '%targets%'#010+
  '{$ifndef ALLPACKAGES}'#010+
  '    Run;'#010+
  '    end;'#010+
  'end.'#010+
  '{$endif ALLPACKAGES}'#010+
  #010
);
