
{$mode objfpc}

type
 { object with vmt }
 pvmtobject = ^tvmtobject;
 tvmtobject = object
   constructor constructor_none;
   destructor destructor_none;
   procedure method_virtual_none; virtual;
   procedure method_static_none; static;
   procedure method_none;
  procedure method_virtual_1; virtual;
 end;
 
 pheritedvmtobject = ^theritedvmtobject;
 theritedvmtobject = object(tvmtobject)
   constructor constructor_none;
   destructor destructor_none;
 end;
 
 { object without vmt }
 pnovmtobject = ^tnovmtobject;
 tnovmtobject = object
   procedure method_none;
   procedure method_static_none; static;
 end;
 
 
 tsampleclass = class
  constructor create;
  destructor destroy;
  procedure method_none;
  class procedure class_method_none;
  class procedure class_method_none_1;
  class constructor create_class;
  class destructor destroy_class;
  procedure method_virtual; virtual;
 end;
 
 
 theritedclass = class(tsampleclass)
   constructor create;
   destructor destroy;
   procedure initialize; virtual;
   procedure finalize; virtual;
 end;
 
 
 procedure tnovmtobject.method_none;
  begin
  end;
  
  
 procedure tnovmtobject.method_static_none;
  begin
  end;
  
  
 constructor tvmtobject.constructor_none;
  begin
  end;
  
  
 procedure tvmtobject.method_virtual_none; 
  begin
  end;
  
 procedure tvmtobject.method_virtual_1;
  begin
    method_virtual_none;
  end;
  
 procedure tvmtobject.method_static_none;
  begin
  end;
  
 procedure tvmtobject.method_none;
   begin
   end;
   
  
destructor tvmtobject.destructor_none;
   begin
   end;
  

   constructor theritedvmtobject.constructor_none;
    begin
      inherited constructor_none;
    end;
    
   destructor theritedvmtobject.destructor_none;
    begin
       inherited destructor_none;
    end;
 
  {*******************************************************************************}
  {                               TSAMPLECLASS                                    }
  {*******************************************************************************}
  constructor tsampleclass.create;
   begin
   end;
   
  destructor tsampleclass.destroy;
   begin
   end;
   
  procedure tsampleclass.method_none;
   begin
   end;
   
   
  procedure tsampleclass.method_virtual; 
   begin
     { methodpointer = nil }
     class_method_none;
   end;
  
  class procedure tsampleclass.class_method_none;
   begin
   end;
   
  class procedure tsampleclass.class_method_none_1;
   begin
     { methodpointer = nil }
     class_method_none;
   end;
   
   
  class constructor tsampleclass.create_class;
   begin
   end;
   
  class destructor tsampleclass.destroy_class;
   begin
   end;
   
  {*******************************************************************************}
  {                               THERITEDCLASS                                   }
  {*******************************************************************************}
   destructor theritedclass.destroy;
    begin
      { generates typen in secondcalln }
      inherited destroy;
    end;
 
   constructor theritedclass.create;
    begin
      { generates typen in secondcalln }
      inherited create;
    end;
 
   procedure theritedclass.initialize; 
     begin
       { methodpointer = nil }
       create;
     end;
     
     
   procedure theritedclass.finalize; 
     begin
       { methodpointer = nil }
       destroy;
     end;
     
   
 function getvmtobject : tvmtobject;
  begin
  end;

var
 myvmtobject : tvmtobject;
var
 sampleclass : tsampleclass;
begin
 Write('typen call node...');
 {****************************** STATIC CALLS ************************************}
{ tvmtobject.method_static_none;
 tnovmtobject.method_static_none;}
 
 { methodpointer : LOC_REGISTER 
   missing methodpointer : LOC_REFERENCE
 }
 {sampleclass := tsampleclass.create;}
 {!!! HOW TO DO A DIRECT CALL TO CLASS DESTRUCTOR/CONSTRUCTOR? }
 tsampleclass.class_method_none;
 sampleclass.class_method_none;
 tsampleclass.destroy_class;
 tsampleclass.create_class;
{ sampleclass.destroy;}
 
 {****************************** CONSTRUCTORS/DESTRUCTORS ************************************}
 

{ OK - full test for instance method done.
  methodpointer : LOC_REFERENCE}
 myvmtobject.constructor_none;
 myvmtobject.destructor_none;
 myvmtobject.method_none;
  {!!!!!!!!1missing : methodpointer : LOC_REGISTER 
 }
 
 {****************************** VIRTUAL METHODS ************************************}
 { !!!!!!!HANDLE SEPARETELY SINCE ITS QUITE COMPLICATED!!!
 myvmtobject.method_virtual_none;}
 
end.

{
  $Log$
  Revision 1.1  2002-04-10 19:11:00  carl
  + first tries at first calln testing for objects (totally unfinished!)

}  