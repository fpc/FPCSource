{$ifdef fpc}{$mode objfpc}{$h+}{$endif}

uses
  Classes;

type
  tbase = class(tobject)
  public
    function add: tobject; overload;
    function add(aitem: tobject): integer; overload;
  end;

  timpl = class(tbase)
  public
    function add: tpersistent; overload;
    function add(aitem: tpersistent): integer; overload;
  end;

var
  err : boolean;
	
function tbase.add: tobject;
begin
  writeln('tbase.add:tobject');
  result := nil;
end;

function tbase.add(aitem: tobject): integer;
begin
  writeln('tbase.add(aitem: tobject)');
  result := -1;
end;

function timpl.add: tpersistent;
begin
  writeln('timpl.add:tpersistent');
  result := nil;
end;

function timpl.add(aitem: tpersistent): integer;
begin
  writeln('timpl.add(aitem: tpersistent)');
	err:=false;
  result := -1
end;

var
  vimpl: timpl;

begin
  err:=true;
  vimpl := timpl.create;
  vimpl.add(nil);
  vimpl.free;
	if err then
	  halt(1);
end.

