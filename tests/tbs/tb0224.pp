{ %version=1.1 }

{ Old file: tbs0262.pp }
{ problems with virtual and overloaded methods }

program test;

  type
      obj1 = object
        st2 : string;
        constructor init;
        procedure writeit;overload;
        procedure writeit(st : string);virtual;overload;
      end;

      obj2 = object(obj1)
        procedure writeit;virtual;overload;
      end;

      obj3 = object(obj2)
        l2 : longint;
        procedure writeit(l : longint);virtual;overload;
        procedure writeit(st : string);virtual;overload;
      end;

      obj4 = object(obj3)
        procedure writeit;virtual;overload;
        procedure writeit(st : string);virtual;overload;
      end;

      obj5 = object(obj4)
        procedure writeit;virtual;overload;
        procedure writeit(st : string);overload;
        procedure writeit(l : longint);virtual;overload;
      end;

      constructor obj1.init;
        begin
        end;

      procedure obj1.writeit;
        begin
          Writeln('Obj1 writeit');
        end;

      procedure obj1.writeit(st : string);
        begin
          Writeln('Obj1 writeit(string) ',st);
        end;

      procedure obj2.writeit;
        begin
          Writeln('Obj2 writeit');
        end;

      procedure obj3.writeit(st : string);
        begin
          Writeln('Obj3 writeit(string) ',st);
        end;

      procedure obj3.writeit(l : longint);
        begin
          Writeln('Obj2 writeit(longint) ',l);
        end;

      procedure obj4.writeit;
        begin
          Writeln('Obj4 writeit');
        end;

      procedure obj4.writeit(st : string);
        begin
          Writeln('Obj4 writeit(string) ',st);
        end;

      procedure obj5.writeit;
        begin
          Writeln('Obj5 writeit');
        end;

      procedure obj5.writeit(st : string);
        begin
          Writeln('Obj5 writeit(string) ',st);
        end;

      procedure obj5.writeit(l : longint);
        begin
          Writeln('Obj5 writeit(longint) ',l);
        end;

var
  o1 : obj1;
  o2 : obj2;
  o3 : obj3;
  o4 : obj4;
  o5 : obj5;



begin
  o1.init;
  o1.writeit;
  o1.writeit('o1');
  o2.init;
  o2.writeit;
  o2.writeit('o2');
  o3.init;
  o3.writeit;
  o3.writeit('o3');
  o3.writeit(3);
  o4.init;
  o4.writeit;
  o4.writeit('o4');
  o4.writeit(4);
  o5.init;
  o5.writeit;
  o5.writeit('o5');
  o5.writeit(5);
end.
