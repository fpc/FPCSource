{ %fail }
{ %CPU=i386 }
{ %target=win32,linux }
{ a method resolution clause on IInterface, an alias of IUnknown, is checked
  as one on IUnknown is (webtbf/tw19591): the mapped method has the wrong
  calling convention. The target must have distinct stdcall and cdecl calling
  conventions, otherwise this test wrongly succeeds }

{$mode objfpc}{$H+}
{$MACRO ON}

type
{$ifdef WINDOWS}
  {$DEFINE extdecl := cdecl}
{$else}
  {$DEFINE extdecl := stdcall}
{$endif}

  TObj = class(TInterfacedObject, IInterface)
    function IInterface._AddRef = AddRef;
    function AddRef: longint; extdecl;
  end;

function TObj.AddRef: longint; extdecl;
begin
  result:=1;
end;

begin
end.
