{ Source provided for Free Pascal Bug Report 2536 }
{ Submitted by "Michael Van Canneyt" on  2003-06-14 }
{ e-mail: Michael.VanCanneyt@wisa.be }
unit tw2536b;

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

