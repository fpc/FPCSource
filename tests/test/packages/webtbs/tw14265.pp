{ %target=linux }
{ %opt=-Xt }

program phello;
{$linklib c}

{$packrecords c}

uses
  ctypes, unixtype, pthreads;

const N = 2;
var
  res:array[1..N] of Integer;

function Hello(arg: pointer): longint; cdecl;
begin
//  writeln('Hello from thread #', PInteger(arg)^);
  res[PInteger(arg)^] := PInteger(arg)^;
  Hello := 0;
  pthread_exit(pointer(Hello));
end;

var
  i: Integer;
  ret: Pointer;
  arg: array[1..N] of Integer;
  threads: array[1..N] of TThreadID;
  attr: TThreadAttr;
begin
  Writeln('Testing simple thread creation');
  pthread_attr_init(attr);
  for i := 1 to N do
  begin
    Writeln('Creating thread #',i);
    arg[i] := i;
    if pthread_create(threads[i], attr, @Hello, @arg[i]) <> 0 then
      Writeln('Failed to create thread');
  end;
  for i := 1 to N do
  begin
    Write('Waiting for thread #',i, ' ... ');
    pthread_join(threads[i], ret);
    Writeln('result: ', res[i]);
  end;
end.


