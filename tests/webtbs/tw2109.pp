{ Source provided for Free Pascal Bug Report 2109 }
{ Submitted by "Layton Davis" on  2002-09-05 }
{ e-mail: layton@brandom.com }
unit tw2109;

{$mode fpc}

interface

{ warning!!! -- pascal re-generates every result in an operator statement }
{   attributes of the results have to be carried forward from the old value }
{   as a work arround we have to ask the user to assign the original destination variable to OldBCD }
{   or OldZoned before doing any assignments or arithmetic }
{ fixme!!! -- assignment statements are used automatically to provide data }
{   type conversion.  I need to provide a safety net so that this doesn't create bad behavior }
{   from this library }


type
  flint = record
    data : longint;
    dec  : byte;
  end;

  bcddata = array[1..18] of char;
  bcd = record
    data   : ^bcddata;
    bcdlen : byte;
    bcddec : byte;
  end;

  zoneddata = array[1..9] of char;
  zoned = record
    data    : ^zoneddata;
    zonelen : byte;
    zonedec : byte;
  end;

operator := (a:bcd)     b:Integer;

operator := (a:bcd)     b:Longint;

operator := (a:bcd)     b:FLInt;

function initbcd(blen, bdec:byte; bcdptr:pointer):bcd;
operator := (a:integer) b:bcd;
operator := (a:longint) b:bcd;
operator := (a:FLInt)   b:bcd;

function initzoned(zlen, zdec:byte; zptr:pointer):zoned;

var
  OldBCD : bcd;

implementation

operator := (a:bcd)     b:Integer;
var
  knt : integer;
begin
  b := 0;
  for knt := 1 to a.bcdlen - a.bcddec do
  begin
    b := b * 10;
    b := b + ord(a.data^[knt]) - ord('0');
  end;
end;

operator := (a:bcd)     b: LongInt;
var
  test : FLInt;
  knt  : byte;
begin
  test := a;
  b := test.data;
  knt := test.dec;
  while knt > 0 do
  begin
    b := b div 10;
    knt := knt - 1;
  end;
end;

operator := (a:bcd)     b:FLInt;
var
  knt : byte;
begin
  b.data := 0;
  for knt := 1 to a.bcdlen do
    b.data := (b.data * 10) + ord(a.data^[knt]) - ord('0');
  b.dec := a.bcddec;
end;

operator := (a:FLInt)   b:bcd;
var
  tmp : FLInt;
  knt : byte;
  tmpl : longint;
begin
  b := oldbcd;
  tmp := a;
  while tmp.dec < b.bcddec do
  begin
    tmp.data := tmp.data * 10;
    tmp.dec := tmp.dec + 1;
  end;
  while tmp.dec > b.bcddec do
  begin
    tmp.data := tmp.data div 10;
    tmp.dec := tmp.dec - 1;
  end;
  for knt := 1 to b.bcdlen do
    b.data^[knt] := '0';
  knt := b.bcdlen;
  while (knt > 0) and (tmp.data > 0) do
  begin
    tmpl := tmp.data div 10;
    tmpl := tmp.data - (tmpl * 10);
    b.data^[knt] := char(ord('0') + tmpl);
    tmp.data := tmp.data div 10;
    knt := knt - 1;
  end;
end;

function initbcd(blen, bdec:byte; bcdptr:pointer):bcd;
var
  temp : bcd;
  knt  : integer;
begin
  if bcdptr <> NIL then
    temp.data := bcdptr
  else
    new(temp.data);
  temp.bcdlen := blen;
  temp.bcddec := bdec;
  for knt := 1 to blen do   {only fill out the space allocated to us -- as we may be part of a data structure}
    temp.data^[knt] := '0';
  initbcd := temp;
end;

operator := (a:integer) b:bcd;
var
  knt : integer;
  temp : integer;
  temp2  : integer;
begin
  b := oldbcd;
  for knt := 1 to b.bcdlen do
    b.data^[knt] := '0';
  knt := b.bcdlen-b.bcddec;
  temp := a;
  while (knt > 0 ) and (temp > 0) do
  begin
    temp2 := temp div 10;
    temp2 := temp - (temp2 * 10);
    temp := temp div 10;
    b.data^[knt] := char(ord('0') + temp2);
    knt := knt - 1;
  end;
end;

operator := (a:longint) b:bcd;
var
  knt : integer;
  temp : longint;
  temp2  : longint;
begin
  b := oldbcd;
  for knt := 1 to b.bcdlen do
    b.data^[knt] := '0';
  knt := b.bcdlen-b.bcddec;
  temp := a;
  while (knt > 0 ) and (temp > 0) do
  begin
    temp2 := temp div 10;
    temp2 := temp - (temp2 * 10);
    temp := temp div 10;
    b.data^[knt] := char(ord('0') + temp2);
    knt := knt - 1;
  end;
end;

function initzoned(zlen, zdec:byte; zptr:pointer):zoned;
begin
end;

end.
