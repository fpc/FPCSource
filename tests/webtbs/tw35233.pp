//
// Submitted by:
//
// Gerrit Moeller
// [private]@gm-software.de
// www.gm-software.de
//

{$MODE DELPHI} // <- Without delphi mode the parser correctly issues an error if stdcall is not repeated
               // in overriding method (see below).

program FPCIntfStdcallOverrideCrash;

type

  ISomeMethod = interface(IUnknown)
    ['{DBFB482B-76FB-4DB7-A321-1001755B1F9E}']
    function SomeMethod(const AIntArg: Integer; const AStrArg: WideString): IUnknown; stdcall;
  end;


  TBaseClassImpl = class(TInterfacedObject, ISomeMethod)
   public
    function SomeMethod(const AIntArg: Integer; const AStrArg: WideString): IUnknown; virtual; stdcall;
  end;


  TDerivedClassImpl = class(TBaseClassImpl)
  public
   //
   // In delphi mode it is not neccessary to repeat stdcall calling convention in the overriding method.
   // But the compiler then generates a call stack with another calling convention for the overriding method!
   // If you call the overriding method through an interface this crashes (SIGSEV) since it is supposed to be stdcall!
   // Repeating the calling convention in the overriding method fixes the crash.
   //
   function SomeMethod(const AIntArg: Integer; const AStrArg: WideString): IUnknown; override; // stdcall; // <- uncommenting stdcall fixes the crash
  end;


function TBaseClassImpl.SomeMethod(const AIntArg: Integer; const AStrArg: WideString): IUnknown; stdcall;
begin
  // Arguments corrupt here due to call stack mismatch!
  Result := nil; // <- SIGSEV crash here due to call stack mismatch!
end;

function TDerivedClassImpl.SomeMethod(const AIntArg: Integer; const AStrArg: WideString): IUnknown;
begin
  // Arguments corrupt here due to call stack mismatch!
 Result := inherited SomeMethod(AIntArg, AStrArg);
end;


procedure Main;
var methodIntf: ISomeMethod; unk: IUnknown;
begin
  methodIntf := TDerivedClassImpl.Create;
  unk := methodIntf.SomeMethod(100, 'Some string contents');
end;


begin
  Main;
end.
