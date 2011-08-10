{ %fail }
{ %CPU=i386 }
{ %target=windows,linux }
{ Target must have distinct stdcall and cdecl calling conventions, otherwise this test will (wrongly) succeed }

{$mode objfpc}{$H+}
{$MACRO ON}

uses
  Classes;

type
// Declare wrong calling convention
{$ifdef WINDOWS}
  {$DEFINE extdecl := cdecl}
{$else}  
  {$DEFINE extdecl := stdcall}
{$endif}  

  { TObj }

  TObj = class(TInterfacedObject, IUnknown)
  
    function IUnknown._AddRef = AddRef;  // This must produce a error because of calling convention mismatch.

    function AddRef : longint;extdecl;
  end;

{ TObj }

function TObj.AddRef: longint;extdecl;
begin
  WriteLn('TObj.AddRef call');
  inherited;
end;

var O:TObj;

begin
  O:=TObj.Create;
  (O as IUnknown)._AddRef;
  O.Free;
end.

