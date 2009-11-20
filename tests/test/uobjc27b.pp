{$mode objfpc}
{$modeswitch objectivec1}

{ Written by Jonas Maebe in 2009, released into the public domain }

unit uobjc27b;

interface

uses uobjc27a;

type
  tachild = objcclass(ta)
  end;

type
  eachild = objccategory(tachild)
    function eachild_categorymethod: longint; message 'eachild_categorymethod';
  end;

  da = objccategory(ta)
    function da_categorymethod: longint; message 'da_categorymethod';
  end;

implementation

function eachild.eachild_categorymethod: longint;
begin
  result:=ca_categorymethod+2;
end;

function da.da_categorymethod: longint;
begin
  result:=2;
end;

end.
