{ %opt=-gh }

program aIntfTest;

{$ifdef fpc}
{$mode delphi}
{$endif}
{$APPTYPE CONSOLE}
uses
  SysUtils, Classes;
 
 
type
  IMyIntf = interface
  ['{34326401-7B67-40FF-8E92-4587F65C8E24}']
    function GetOwner: IMyIntf;
    procedure Poing;
  end;

type
  TMYClass = clasS(TinterfacedObject, IMyIntf)
    fRef: Integer;
  public
    function GetOwner: IMyIntf;
    function QueryInterface(constref IID: TGUID; out Obj): HRESULT; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: Integer; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    procedure Poing;
  end;
 
{ TMYClass }
 
function TMYClass._AddRef: Integer;
begin
  inc(fRef);
  result := fRef;
  Writeln('AddRef:'+inttostr(result));
end;
 
function TMYClass._Release: Integer;
begin
  Dec(fRef);
  result := FRef;
  Writeln('Release:'+inttostr(result));
  if result = 0 then Free;
end;
 
function TMYClass.GetOwner: IMyIntf;
begin
  Writeln('GetOwner1');
  result := nil;
  Writeln('GetOwner2');
end;
 
function TMYClass.QueryInterface(constref IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    result := S_OK else result := -1;
end;
 
var
  r: IMyIntf;

procedure Test(x: IMyIntf);
begin
  if x <> nil then x.Poing;
  x := x.GetOwner;
  if x <> nil then x.Poing;
end;

procedure TMYClass.Poing;
begin
  writeln('poing');
end;

begin
  HaltOnNotReleased := true;
  r := TMYClass.Create;
  Test(r);
  Writeln('nil');
  r := nil; 
end.
