{ %fail }

{$mode delphi}
program test2;

type
  XResult = integer;
  XStr = integer;

  PDNSEntry = ^XDNSEntry;
  XDNSEntry =
  record
  end;

  itest = interface(iunknown)
    function GetHostByName(const Name: XStr; out Res: PDNSEntry): XResult;
    function GetHostByName(const Name: XStr; out Res: XDNSEntry): XResult;
  end;

begin
end.
