program tstringhelperjoin;

{$ifndef fpc}
{$APPTYPE CONSOLE}
{$else}
{$mode delphi}
{$endif}

uses
{$ifndef fpc}  System.{$endif}SysUtils;

var testsuccess : boolean;

procedure dojoin(const testname,shouldbe:string;sep:string;some:array of string;start,cnt : integer;isexception:boolean);
var s : string;
    res:  boolean;
begin
 res:=false;
 try
   s:=s.Join(sep,some,start,cnt);
  except
    on e : Erangeerror do
    res:=true;
 end;
 if isexception and not res then
  begin
    testsuccess :=false;
    writeln(testname,' FAIL on rangeexception NOT happening while it should')
  end
 else
   if not isexception and res then
     begin
       testsuccess :=false;
       writeln(testname,' FAIL, rangeexception while it shouldn''t')
     end
   else
     if not res and (s<>shouldbe) then
       begin
         testsuccess :=false;
         writeln(testname,' FAIL on result mismatch ' ,s,'  should be ',shouldbe);
       end
     else
        writeln(testname,' ok');
end;

begin
  testsuccess :=true;
  dojoin('default number','String1,String2,String3', ',', ['String1', 'String2', 'String3'],0,3,false);
  dojoin('other sep','String2AAString3', 'AA', ['String1', 'String2', 'String3'],1,2,false);
  dojoin('index not 0','String2,String3', ',', ['String1', 'String2', 'String3'],1,2,false);
  dojoin('no data ','', ',', [],1,2,true);
  dojoin('both 0 ','', ',', [],1,0,false);
  dojoin('count 0','', ',', ['String1', 'String2', 'String3'],1,0,false);
  dojoin('index not 0 overflow','String2,String3', ',', ['String1', 'String2', 'String3'],1,5,false);
  dojoin('exception large start','String1,String2,String3', ',', ['String1', 'String2', 'String3'],4,3,true);
  dojoin('exception large count','String1,String2,String3', ',', ['String1', 'String2', 'String3'],4,10,true);

{$ifndef fpc}
  if debughook>0 then
    readln;
{$endif}
  if not testsuccess then
    halt(1)
  else
    halt(0);
end.

