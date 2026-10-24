{ %TARGET=linux,freebsd,netbsd,openbsd,dragonfly }
{ %needlibrary }
{ %delfiles=treslib1a }
{ FindResource on the handle of a library that exports rsrc finds the library's resources, not the program's }
program treslib1b;

{$mode objfpc}

uses
  SysUtils, DynLibs;

{$R tres4.res}

var
  gErrors: Integer = 0;


procedure CheckTrue(const aName: string; aValue: Boolean);

begin
  if aValue then
    Writeln(aName,' OK')
  else
    begin
    Writeln(aName,' ERROR');
    Inc(gErrors);
    end;
end;


// The contents of resource aName of type aType in module aModule, or '' if it is not found.
function GetResourceText(aModule: TFPResourceHMODULE; aName, aType: PAnsiChar): AnsiString;

var
  lRes: TFPResourceHandle;
  lGlobal: TFPResourceHGLOBAL;

begin
  Result:='';
  lRes:=FindResource(aModule,aName,aType);
  if lRes=0 then
    Exit;
  lGlobal:=LoadResource(aModule,lRes);
  if lGlobal=0 then
    Exit;
  SetString(Result,PAnsiChar(LockResource(lGlobal)),SizeofResource(aModule,lRes));
  UnlockResource(lGlobal);
  FreeResource(lGlobal);
end;


var
  lLibName: string;
  lLib: TLibHandle;

begin
  lLibName:=ExtractFilePath(ParamStr(0))+'libtreslib1a.'+SharedSuffix;
  lLib:=LoadLibrary(lLibName);
  if lLib=NilHandle then
    begin
    Writeln('Cannot load ',lLibName);
    Halt(1);
    end;
  try
    CheckTrue('Library exports rsrc', GetProcAddress(lLib,'rsrc')<>nil);
    CheckTrue('Library FILE resource', GetResourceText(lLib,'TestFile','FILE')='test file.');
    CheckTrue('Library TEXT resource', GetResourceText(lLib,'Test','TEXT')='Another test file.');
    CheckTrue('Program resource not found in library', FindResource(lLib,'mdtoptyt_png',RT_RCDATA)=0);
    CheckTrue('Library resource not found in program', FindResource(HInstance,'TestFile','FILE')=0);
    CheckTrue('Program resource found in program', Length(GetResourceText(HInstance,'mdtoptyt_png',RT_RCDATA))=136303);
  finally
    UnloadLibrary(lLib);
  end;
  Writeln(gErrors,' errors');
  Halt(gErrors);
end.
