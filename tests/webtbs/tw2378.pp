{ Source provided for Free Pascal Bug Report 2378 }
{ Submitted by "Yakov Sudeikin" on  2003-02-13 }
{ e-mail: yashka@exebook.com }

{$mode delphi}

type
 tfunc = procedure of object;

 ttest = class
   procedure callback;
   procedure start;
   procedure call(ptr: tfunc);
 end;

procedure ttest.callback;
begin
end;

procedure ttest.call;
begin
end;

procedure ttest.start;
begin
 call(callback);
end;

begin
end.
