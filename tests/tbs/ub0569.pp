{ %norun }
{$mode objfpc}
unit ub0569;

  interface

    type
      generic TGen<T> = class
        function getstring : string;
        function getwidestring : widestring;
        function getint : int64;
        function getreal : real;
      end;

  implementation

    function TGen<T>.getstring : string;
      begin
        result:='Free Pascal';
      end;


    function TGen<T>.getwidestring : widestring;
      begin
        { force widestring }
        result:='Free Pascal'#1234;
      end;


    function TGen<T>.getint : int64;
      begin
        result:=1234123412341234;
      end;


    function TGen<T>.getreal : real;
      begin
        result:=333.0;
      end;


end.

