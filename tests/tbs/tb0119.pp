{ Old file: tbs0139.pp }
{ Cannot access protected method of ancestor class from other unit. OK 0.99.6 }

unit tb0119;

{$mode objfpc}

 interface
 uses
    ub0119;

 type
    AnotherClass=class(SomeClass)
    protected
    procedure doSomething; override;
    end ;

 implementation

 procedure AnotherClass.doSomething;
 begin
 inherited doSomething;  // this causes the error: " can not call protected
                         //  method from here " ( or something similar )
 end ;

end.
