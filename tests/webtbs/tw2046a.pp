{ Source provided for Free Pascal Bug Report 2046 }
{ Submitted by "Mattias Gaertner" on  2002-07-17 }
{ e-mail: nc-gaertnma@netcologne.de }

{ modified to check that passing array of const
  from functions to functions works correctly PM }

program printftest;

{$mode objfpc}{$H+}

uses
  sysutils;


procedure sprintf(var a : string; const fm: string; args: array of const);
begin
  a:=Format (fm,args);
end;

procedure sprintf2(var a : string; const fm: string; args: array of const);
begin
  FmtStr (a,fm,args);
end;

procedure print(args: array of const);
var
  a : string;
begin
  sprintf(a,'a number %D-%D%S',args);
  writeln(a);
  if a<>'a number 3333-45 test string' then
   begin
     writeln('Error!');
     halt(1);
   end;
  sprintf2(a,'a number %D-%D%S',args);
  writeln(a);
  if a<>'a number 3333-45 test string' then
   begin
     writeln('Error!');
     halt(1);
   end;
end;

var
  a : string;
begin
  a:=format('a number %D',[12345]);
  writeln(a);
  if a<>'a number 12345' then
   begin
     writeln('Error!');
     halt(1);
   end;
  print([3333,45,' test string']);
end.
