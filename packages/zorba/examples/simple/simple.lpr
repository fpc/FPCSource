program simple;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  zorba,
  ctypes;

{$IFDEF WINDOWS}{$R simple.rc}{$ENDIF}

procedure stream_write(stream: XQC_OutputStream; const buf: pchar; length: cuint); cdecl;
var
  S: String;
begin
  SetLength(S, length);
  Move(buf^, S[1], length);
  WriteLn(S);
end;

procedure stream_free(stream: XQC_OutputStream); cdecl;
begin
end;

const
  streamdef: XQC_OutputStream_s = (
    write:@stream_write;
    free:@stream_free;
    data:nil
  );

var
  stream: XQC_OutputStream = @streamdef;




(**
 * A simple C API example
 * Compile a query and print the result.  No error checking is done.
 *)
function example_1(impl: XQC_Implementation): boolean;
var
  lXQuery: XQC_Query;
begin
  // compile the query
  impl^.prepare(impl, '(1+2, 3, 4)', nil, nil, lXQuery);

  // execute it and print the result on standard out
  lXQuery^.serialize_stream(lXQuery, nil, stream);

  // release the query
  lXQuery^.free(lXQuery);

  Result := True;
end;



(**
 * A simple C API example
 * Compile a query, iterate over the item sequence, and print the string value for each item.
 * No error checking is done.
 *)
function example_2(impl: XQC_Implementation): boolean;
var
  lXQuery      : XQC_Query;
  lItem        : XQC_Item;
  lResult      : XQC_Sequence;
  lStringValue : pchar;
begin
  impl^.create_item(impl, lItem);

  // compile the query and get the result as a sequence
  impl^.prepare(impl, 'for $i in 1 to 10 return $i', nil, nil, lXQuery);

  lXQuery^.sequence(lXQuery, lResult);

  while lResult^.next(lResult, lItem) = XQ_NO_ERROR do
  begin
    lItem^.string_value(lItem, &lStringValue);
    write(lStringValue, ' ');
  end;
  writeln;

  // release all aquired resources
  lItem^.free(lItem);
  lResult^.free(lResult);
  lXQuery^.free(lXQuery);

  Result := True;
end;



var
  impl: XQC_Implementation;
  store: Pointer;
begin
  store := create_simple_store();
  if zorba_implementation(impl, store) <> XQ_NO_ERROR then
    Exit;

  writeln('executing PASCAL example 1');
  if not example_1(impl) then
    Exit;
  writeln;

  writeln('executing PASCAL example 2');
  if not example_2(impl) then
    Exit;
  writeln;

  impl^.free(impl);
  shutdown_simple_store(store);
end.

