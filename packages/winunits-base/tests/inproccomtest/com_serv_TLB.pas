unit com_serv_TLB;
// part of Comtest demo from Anton K. mantis #35013

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 16.08.2019 18:46:07 from Type Library described below.

// ************************************************************************  //
// Type Lib: com_serv.tlb (1)
// LIBID: {4657B1E3-77D1-4504-A96C-3E79EF05721C}
// LCID: 0
// Helpfile: 
// HelpString: Project1 Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface
{$ifdef fpc}{$mode delphi}{$endif}

uses Windows, ActiveX, Classes, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  com_servMajorVersion = 1;
  com_servMinorVersion = 0;

  LIBID_com_serv: TGUID = '{4657B1E3-77D1-4504-A96C-3E79EF05721C}';

  IID_ITestApp: TGUID = '{1DD0AE6B-30C7-474E-8972-01981454B649}';
  CLASS_TestApp: TGUID = '{FD2054C2-4C67-47AE-A518-3FA6A7D691AA}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  ITestApp = interface;
  ITestAppDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  TestApp = ITestApp;


// *********************************************************************//
// Interface: ITestApp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1DD0AE6B-30C7-474E-8972-01981454B649}
// *********************************************************************//
  ITestApp = interface(IDispatch)
    ['{1DD0AE6B-30C7-474E-8972-01981454B649}']
    procedure test(const text: WideString); safecall;
    procedure test_ret(var res: OleVariant); safecall;
  end;

// *********************************************************************//
// DispIntf:  ITestAppDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {1DD0AE6B-30C7-474E-8972-01981454B649}
// *********************************************************************//
  ITestAppDisp = dispinterface
    ['{1DD0AE6B-30C7-474E-8972-01981454B649}']
    procedure test(const text: WideString); dispid 201;
    procedure test_ret(var res: OleVariant); dispid 202;
  end;

// *********************************************************************//
// The Class CoTestApp provides a Create and CreateRemote method to          
// create instances of the default interface ITestApp exposed by              
// the CoClass TestApp. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoTestApp = class
    class function Create: ITestApp;
    class function CreateRemote(const MachineName: string): ITestApp;
  end;

implementation

uses ComObj;

class function CoTestApp.Create: ITestApp;
begin
  Result := CreateComObject(CLASS_TestApp) as ITestApp;
end;

class function CoTestApp.CreateRemote(const MachineName: string): ITestApp;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_TestApp) as ITestApp;
end;

end.
