program tprop;

{$mode delphi}

uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

type
  tc = class
   strict private
    fvalue: longint;
    function getit: longint;
    procedure setit(l: longint);
   public
    property value: longint read getit write setit;
    constructor create(l: longint);
  end;

  constructor tc.create(l: longint);
    begin
      fvalue:=l;
    end;


  function tc.getit: longint;
    begin
      result:=fvalue;
    end;


  procedure tc.setit(l: longint);
    begin
      fvalue:=l;
    end;

var
  c: tc;
begin
  c:=tc.create(5);
  jlsystem.fout.println(c.value);
  c.value:=6;
  jlsystem.fout.println(c.value);
end.
