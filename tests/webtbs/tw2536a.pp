{ Source provided for Free Pascal Bug Report 2536 }
{ Submitted by "Michael Van Canneyt" on  2003-06-14 }
{ e-mail: Michael.VanCanneyt@wisa.be }
unit tw2536a;

interface

Type
  TWSAData = Pointer;

var

  // Delphi accepts this.
  WSAStartup: function(wVersionRequired: Word; var WSData: TWSAData): Integer stdcall = nil;
  // FPC should accept this too
  WSAStartup2: function(wVersionRequired: Word; var WSData: TWSAData): Integer;stdcall = nil;

implementation

end.