{ %FAIL }

procedure something;
 procedure SomethingExt (onepar:longint); external;
begin
 SomethingExt (1);
end;

begin
end.
