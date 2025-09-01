unit u41384a;

{$mode ObjFPC}
{$H+}
{$modeswitch advancedrecords}

interface

uses
  classes, sysutils;

type

  TAddressType = (atIN4, atIN6, atUnixSock);
  TNetworkAddress = record
    Address: String;
    AddressType: TAddressType;
  end;

function isIPv4Address(const Address: String): Boolean; inline;
function isIPv6Address(const Address: String): Boolean; inline;

function NetAddr(const Address: String): TNetworkAddress;inline;
function DefAddr : TNetworkAddress;

implementation

function do_count(const Address:String; sep: char): Word;
var
  i : longint;
begin
  do_count:=0;
  for i:=1 to Length(Address) do
    begin
      if (Address[i]=sep) then
        inc(do_count);
    end;
end;

function isIPv4Address(const Address:String):Boolean;
begin
  Result := do_count(Address,'.')=3;
end;

function isIPv6Address(const Address:String):Boolean;
begin
  Result := do_count(Address,':')=5;
end;

{$ifndef SHOW_FIX}
function DefAddr : TNetworkAddress;
begin
  DefAddr:=Default(TNetworkAddress);
end;
{$endif}

function NetAddr(const Address: String): TNetworkAddress;
begin
 Result := Default(TNetworkAddress);
  if isIPv4Address(Address) then
    Result.AddressType := atIN4
  else if isIPv6Address(Address) then
    Result.AddressType := atIN6
  else // Filenames can be pretty much anything
    Result.AddressType := atUnixSock;
  Result.Address := Address;
end;

{$ifdef SHOW_FIX}
function DefAddr : TNetworkAddress;
begin
  DefAddr:=Default(TNetworkAddress);
end;
{$endif}

end.
