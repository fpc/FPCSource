{$ifdef FPC}{$mode objfpc}{$endif}

function kek_absolute_inline: Single; inline;
var
  r: UInt32 absolute result;
begin
  result:=0;
  r := 777;
end;

function kek_absolute: Single;
var
  r: UInt32 absolute result;
begin
  result:=0;
  r := 777;
end;

function kek: Single;
begin
  puint32(@result)^:=777;
end;

function kek_inline: Single; inline;
begin
  puint32(@result)^:=777;
end;

var
  error_count : byte;

procedure print(name: String; value: Single);
var
  i : uint32;
begin
  i:=puint32(@value)^;
  WriteLn(name, i);
  if i<>777 then
    inc(error_count);
end;

begin
  error_count:=0;
  print('kek:                 ', kek);                  // 777
  print('kek_inline:          ', kek_inline);           // 777
  print('kek_absolute:        ', kek_absolute);         // 777
  print('kek_absolute_inline: ', kek_absolute_inline);  // 0
  if error_count>0 then
    begin
      writeln('Test fails');
      halt(error_count);
    end
  else
    writeln('Test completed successfully!');
end.

