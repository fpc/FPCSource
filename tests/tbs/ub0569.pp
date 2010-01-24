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

    function TGen.getstring : string;
      begin
        result:='Free Pascal';
      end;


    function TGen.getwidestring : widestring;
      begin
        { force widestring }
        result:='Free Pascal'#1234;
      end;


    function TGen.getint : int64;
      begin
        result:=1234123412341234;
      end;


    function TGen.getreal : real;
      begin
        result:=333.0;
      end;


end.

