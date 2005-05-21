{ %target=linux,freebsd }

{ Source provided for Free Pascal Bug Report 3257 }
{ Submitted by "peter green" on  2004-08-18 }
{ e-mail: plugwash@p10link.net }
unit tw3257;

{$mode delphi}
{$inline on}

interface

implementation
uses
  baseunix,unix,sysutils;

  procedure gettimeofday(var tv:ttimeval);inline;
    begin
      fpgettimeofday(@tv,nil);
    end;
    function gettimeofdaysec : longint;inline;
    var
      tv:ttimeval;
    begin
      gettimeofday(tv);
      result := tv.tv_sec;
    end;
end.
