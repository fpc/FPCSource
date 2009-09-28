{ %norun }

{ This is just a small file used to verify the alignment of different
  structures. Only the assembler output should be checked.
}

{$mode objfpc}

type
  tmyclass = class
    s: ansistring;
    f: real;
  end;
{$M+}
  tmyotherclass = class(tmyclass)
  public
    procedure tito(var Msg); message 'hello';
    procedure tita(var Msg); message 'h';
    procedure titi(var Msg); message 12;
  published
    procedure published_method;
  end;

  procedure tmyotherclass.tito(var Msg);
   begin
   end;

  procedure tmyotherclass.tita(var Msg);
   begin
   end;

  procedure tmyotherclass.titi(var Msg);
   begin
   end;

  procedure tmyotherclass.published_method;
   begin
   end;


var
 c: tmyclass;
Begin
end.
