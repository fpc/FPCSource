{$mode objfpc}
{$H+}
{ Define FullFCL, this way we know it's a complete FCL build} 
{$DEFINE FULLFCL} 
program fpmake;

uses fpmkunit;

Var
  T : TTarget;
   
begin
  With Installer do 
    begin
    { general definitions }
    {$i fclmake.inc}

    { Basic targets. }
    {$i inc/fpmake.inc}
    
    { XML directory }
    {$i xml/fpmake.inc}
    
    { Image directory }
    {$i image/fpmake.inc}
    
    { db directory }
    {$i db/fpmake.inc}
    
    { Shedit directory }  
    {$i shedit/fpmake.inc}

    { Passrc directory }
    {$i passrc/fpmake.inc}
    
    { Net directory }
    {$i net/fpmake.inc}

    { fpcunit directory }
    {$i fpcunit/fpmake.inc}

    Targets.ResetDefaults;
    
    { All done.}
    EndPackage;
    Run;
    end;
end.

