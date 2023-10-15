{%NORUN}

program Project1;

  {$mode delphi}

type
  TEnum = (A, B, C);

  procedure Test<T>(E: T);
  type
    S1 = set of TEnum;
    S2 = set of Low(T)..High(T);
    S3 = set of T; //Error
  begin
  end;

begin
  Test<TEnum>(B);
end.      
