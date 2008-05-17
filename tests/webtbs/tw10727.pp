{$ifdef fpc} {$mode delphi}{$endif}
Type
   TStringBuilder = Class
    function Insert(Index: Integer; const Value: string; Count: Integer = 1): TStringBuilder; overload;
    function Insert(Index: Integer; const Value: array of Char): TStringBuilder; overload;
end;

function TStringBuilder.Insert(Index: Integer; const Value: array of Char): TStringBuilder;
begin
 writeln('Called TStringBuilder.Insert(Index: Integer; const Value: array of Char): TStringBuilder;');
 result:=nil;
end;

function TStringBuilder.Insert(Index: Integer; const Value: string; Count: Integer): TStringBuilder;
begin
  writeln('Called TStringBuilder.Insert(Index: Integer; const Value: string; Count: Integer): TStringBuilder;');
 result:=nil;
end;

var sb : TSTringBuilder;

begin
  sb:=TStringBuilder.Create;
  sb.Insert(0, '0 ');
  sb.Free;
end.
