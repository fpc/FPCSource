program importtl;

{$mode objfpc}{$H+}
{$apptype console}
uses
  classes,typelib,sysutils,getopts;

var
  theopts : array[1..2] of TOption;

procedure InitOptions;

begin
  with theopts[1] do
   begin
    name:='ref-style';
    has_arg:=Required_Argument;
    flag:=nil;
    value:=#0;
  end;
  with theopts[2] do
   begin
    name:='';
    has_arg:=0;
    flag:=nil;
    value:=#0;
  end;
end;

var
  unitname,sPackageSource,sPackageRegUnitSource:string;
  sTL,sOutDir:string;
  F:text;
  slDep:TStringList;
  i:integer;
  FileName : string;
  bNoRecurse,bHelp,bActiveX,bPackage,bRemoveStructTag:boolean;
  InRefStyle : TParamInputRefType;
  optionindex : Longint;
  c:char;
begin
  InitOptions;
  slDep:=TStringList.Create;
  bNoRecurse:=false;
  bHelp:=false;
  bActiveX:=false;
  bPackage:=false;
  InRefStyle:=ParamInputVar;

  repeat
    c:=getlongopts('ad:hnpt',@theopts[1],optionindex);
    case c of
       #0 : begin
             case optionindex-1 of
               0 : if lowercase(optarg)='var' then
                     InRefStyle:=ParamInputVar
                   else
                    if lowercase(optarg)='constref' then
                     InRefStyle:=ParamInputConstRef
                   else
                     if lowercase(optarg)='constrefdelphi' then
                       InRefStyle:=ParamInputConstRefDelphi
              end;
           end;
      'n' : bNoRecurse:=true;
      'a' : bActiveX:=true;
      'p' : bPackage:=true;
      'h' : bHelp:=true;
      't' : bRemoveStructTag:=true;
      'd' :  if (length(optarg)>0) and (optarg[1]='-') then
                bHelp:=true
              else
                sOutDir:=IncludeTrailingPathDelimiter(optarg);
      '?',':' : writeln ('Error parsing option : ',optopt);
   end; { case }
 until c=endofoptions;

 FileName:='';
 if optind=paramcount then
   FileName:=paramstr(optind);

 if bHelp or (Paramcount=0) or (filename='')then
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
    writeln('  --ref-style st : input parameter style, parameter st=var,constref');
    writeln('            or constrefdelphi (= XE3+ const [ref])');
    halt;
    end;
  slDep.Add(paramstr(Paramcount));
  i:=0;
  repeat
    writeln('Reading typelib from '+slDep[i]+ ' ...');
    sTL:=ImportTypelib(slDep[i],unitname,slDep,bActiveX,bPackage,bRemoveStructTag,sPackageSource,sPackageRegUnitSource,InRefStyle);
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

