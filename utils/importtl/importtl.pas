program importtl;

{$mode objfpc}{$H+}
{$apptype console}
uses
  classes,typelib,sysutils;

var
  unitname,sPackageSource,sPackageRegUnitSource:string;
  sTL,sOutDir:string;
  F:text;
  slDep:TStringList;
  i:integer;
  bNoRecurse,bHelp,bActiveX,bPackage,bRemoveStructTag:boolean;
begin
  slDep:=TStringList.Create;
  bNoRecurse:=false;
  bHelp:=false;
  bActiveX:=false;
  bPackage:=false;
  i:=1;
  while i<=Paramcount do
    begin
    if pos('-n',ParamStr(i))>0 then bNoRecurse:=true
    else if pos('-a',ParamStr(i))>0 then bActiveX:=true
    else if pos('-h',ParamStr(i))>0 then bHelp:=true
    else if pos('-p',ParamStr(i))>0 then bPackage:=true
    else if pos('-t',ParamStr(i))>0 then bRemoveStructTag:=true
    else if pos('-d',ParamStr(i))>0 then
      begin
      sOutDir:=trim(copy(ParamStr(i), pos('-d',ParamStr(i))+2, 260));  // windows MAX_PATH
      if sOutDir='' then
        if i<Paramcount-1 then
          begin
          i:=i+1;
          sOutDir:=trim(ParamStr(i));
          end
        else
          begin
          bHelp:=true;
          sOutDir:='\';
          end;
      if not (sOutDir[length(sOutDir)] in [':','\']) then
        sOutDir:=sOutDir+'\';
      end;
    i:=i+1;
    end;
  if bHelp or (Paramcount=0) or (pos('-',paramstr(Paramcount))=1) then
    begin
    writeln('Usage:  importtl [options] file');
    writeln('Reads type information from "file" and converts it into a freepascal binding');
    writeln('units.');
    writeln('Options.');
    writeln('  -h    : displays this text.');
    writeln('  -a    : create ActiveXContainer descendants');
    writeln('  -d dir: set output directory. Default: current directory.');
    writeln('  -n    : do not recurse typelibs. Default: create bindings for all');
    writeln('          dependencies.');
    writeln('  -p    : create lazarus package for ActiveXContainer descendants');
    writeln('  -t    : remove "tag" prefix from structs');
    exit;
    end;
  slDep.Add(paramstr(Paramcount));
  i:=0;
  repeat
    writeln('Reading typelib from '+slDep[i]+ ' ...');
    sTL:=ImportTypelib(slDep[i],unitname,slDep,bActiveX,bPackage,bRemoveStructTag,sPackageSource,sPackageRegUnitSource);
    unitname:=sOutDir+unitname;
    if (bPackage) and (sPackageSource<>'') then
      begin
      writeln('Writing package file to '+unitname+'P.lpk' );
      AssignFile(F,unitname+'P.lpk');
      Rewrite(F);
      Write(F,sPackageSource);
      CloseFile(F);
      writeln('Writing package registration file to '+unitname+'Preg.pas');
      AssignFile(F,unitname+'Preg.pas');
      Rewrite(F);
      Write(F,sPackageSource);
      CloseFile(F);
      end;
    bActiveX:=false;  //don't create ActiveXContainer descendants in descendants
    bPackage:=false;
    writeln('Writing to '+unitname+'.pas');
    AssignFile(F,unitname+'.pas');
    Rewrite(F);
    Write(F,sTL);
    CloseFile(F);
    i:=i+1;
  until bNoRecurse or (i=slDep.Count);
  slDep.Destroy;
end.

