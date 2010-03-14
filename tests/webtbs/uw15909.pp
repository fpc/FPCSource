unit uw15909;
{$mode Delphi}

{$inline on}

interface

    procedure foo(const s: widestring; const n: integer); inline;

    function bar(const s, fmt: widestring): integer;

implementation

procedure foo(const s: widestring; const n: integer);
begin
    bar(s, '%d')
end;


    function bar(const s, fmt: widestring): integer;
      begin
        if (s<>'abc') or
           (fmt<>'%d') then
          begin
            writeln('"',s,'"');
            halt(1);
          end;
        result:=0;
      end;

end.
