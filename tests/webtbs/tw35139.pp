{ 
  Check a fix for a bug that appeared in
  utils/fppkg/lnet/lTelnet.pp 
}

{$mode objfpc}{$H+}

unit tw35139;

interface

uses
  Classes, SysUtils;

implementation

var
  zz: Char;
  TNames: array[Char] of string;
initialization
  for zz := #0 to #255 do
    TNames[zz] := IntToStr(Ord(zz));
  TNames[#1] := 'TS_ECHO';
  TNames[#133] := 'TS_HYI';
  TNames[#251] := 'TS_WILL';
  TNames[#252] := 'TS_WONT';
  TNames[#253] := 'TS_DO';
  TNames[#254] := 'TS_DONT';
end.

