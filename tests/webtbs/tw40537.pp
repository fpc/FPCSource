type
{$ifdef USE_RECORD}
  trec = record
    y : longint;
  end;
{$else}
{$ifdef USE_PTRINT}
  trec = ptrint;
{$else USE_PTRINT}
  trec = pointer;
{$endif USE_PTRINT}
{$endif}
  prec = ^trec;

const
  value_version_used : longint = 0;
  var_version_used : longint = 0;
  has_error : boolean = false;

function test(p : prec;l : longint; k: dword) : boolean; overload;
begin
  test:=(p<>nil);
  inc(value_version_used);
end;

function test(var p : trec;l : longint; k: dword) : boolean; overload;
begin
  test:=(@p<>nil);
  inc(var_version_used);
end;

var
  pt : trec;
  i : trec;

begin
  pt:=i;
  test(@pt,23,56);
  if (var_version_used>0) then
    begin
      writeln('call with @pt uses var version, which is wrong');
      has_error:=true;
    end
  else
    writeln('call with @pt uses value version');

  var_version_used:=0;
  value_version_used:=0;

  test(pt,678,567890);
  if (var_version_used>0) then
    writeln('direct call uses var version')
  else
    writeln('direct call uses value version');

  if has_error then
    begin
      writeln('This test revealed a problem');
      halt(1);
    end;
end.

