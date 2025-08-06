{ Source provided for Free Pascal Bug Report 2536 }
{ Submitted by "Michael Van Canneyt" on  2003-06-14 }
{ e-mail: Michael.VanCanneyt@wisa.be }
unit tw2536c;

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
