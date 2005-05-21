{ Source provided for Free Pascal Bug Report 2536 }
{ Submitted by "Michael Van Canneyt" on  2003-06-14 }
{ e-mail: Michael.VanCanneyt@wisa.be }
unit tw2536;

interface

Type
  TWSAData = Pointer;

var

  // Delphi accepts this.
  WSAStartup: function(wVersionRequired: Word; var WSData: TWSAData): Integer stdcall = nil;
  // FPC should accept this too
  WSAStartup2: function(wVersionRequired: Word; var WSData: TWSAData): Integer;stdcall = nil;

implementation

end.unit testu2;

interface

Type
  TWSAData = Pointer;

const

  // FPC and Delphi accepts this.
  WSAStartup: function(wVersionRequired: Word; var WSData: TWSAData): Integer stdcall = nil;
  // FPC does not accept this.
  WSAStartup2: function(wVersionRequired: Word; var WSData: TWSAData): Integer = nil; stdcall;

implementation
end.
unit testu3;
interface
Type
  TWSAData = Pointer;
  TStartupFunction = function(wVersionRequired: Word; var WSData: TWSAData): Integer stdcall;
  TStartupFunction2 = function(wVersionRequired: Word; var WSData: TWSAData): Integer; stdcall;

var

  WSAStartup: TStartupFunction = nil;
  WSAStartup2: TStartupFunction2 = Nil;

implementation

end.
