{$ifdef fpc}{$mode objfpc}{$endif}

uses classes;

var SL : TStringlist;

begin
  SL := TStringlist.Create;
  sl.Add('1');
  sl.Add('2');
  sl.Add('A"A');
  sl.Add('B');
  sl.Add('C,C');
  sl.Add('D;D');
  writeln(sl.Text);
  writeln(sl.CommaText);
  if sl.CommaText<>'1,2,"A""A",B,"C,C",D;D' then
   begin
     writeln('ERROR!');
     halt(1);
   end;
  sl.free;
end.
