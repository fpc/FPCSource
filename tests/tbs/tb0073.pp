{ Old file: tbs0080.pp }
{  Shows Missing High() (internal) function.             OK 0.99.6 (MVC) }

program bug0080;

type

 tHugeArray = array [ 1 .. High(Word) ] of byte;

begin
end.
