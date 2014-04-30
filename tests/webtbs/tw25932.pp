{$mode delphi}

uses
  sysutils;

procedure testfile;
var
  f: file;
  s: shortstring;
  a: ansistring;
  u: unicodestring;
begin
  s:='a';
  a:='b';
  u:='c';

  fillchar(f,sizeof(f),0);
  try
    erase(f);
  except
    on e: EInOutError do
     if e.ErrorCode<>102 then
       raise
  end;

  fillchar(f,sizeof(f),0);
  try
    rename(f,s);
  except
    on e: EInOutError do
     if e.ErrorCode<>102 then
       raise
  end;

  fillchar(f,sizeof(f),0);
  try
    rename(f,a);
  except
    on e: EInOutError do
     if e.ErrorCode<>102 then
       raise
  end;

  fillchar(f,sizeof(f),0);
  try
    rename(f,u);
  except
    on e: EInOutError do
     if e.ErrorCode<>102 then
       raise
  end;
end;

procedure testtext;
var
  f: text;
  s: shortstring;
  a: ansistring;
  u: unicodestring;
begin
  s:='a';
  a:='b';
  u:='c';

  fillchar(f,sizeof(f),0);
  try
    erase(f);
  except
    on e: EInOutError do
     if e.ErrorCode<>102 then
       raise
  end;

  fillchar(f,sizeof(f),0);
  try
    rename(f,s);
  except
    on e: EInOutError do
     if e.ErrorCode<>102 then
       raise
  end;

  fillchar(f,sizeof(f),0);
  try
    rename(f,a);
  except
    on e: EInOutError do
     if e.ErrorCode<>102 then
       raise
  end;

  fillchar(f,sizeof(f),0);
  try
    rename(f,u);
  except
    on e: EInOutError do
     if e.ErrorCode<>102 then
       raise
  end;
end;

begin
  testfile;
end.

