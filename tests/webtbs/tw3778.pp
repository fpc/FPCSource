{ Source provided for Free Pascal Bug Report 3778 }
{ Submitted by "David Fuchs" on  2005-03-13 }
{ e-mail: drfuchs@yahoo.com }

{$mode objfpc}

type
  a_class=class
    procedure a_method;
    function a_virtual_method:a_class; virtual;
    end;
  a_container=class
    a_field:a_class;
    end;

var
  already_called: boolean;
  glob: a_class;
  container_array: array[0..255] of a_container;
        error : boolean;

function a_function:byte;
begin
  if already_called then
    begin
      writeln('This can not possibly happen!');
      error:=true;
    end;
  a_function:=255;
  already_called:=true;
end;

function a_class.a_virtual_method:a_class;
begin
  a_virtual_method:=self;
end;

procedure a_class.a_method;
begin
  {this statement somehow compiles into TWO calls to a_function!}
  glob:=container_array[a_function].a_field.a_virtual_method;
end;

begin
        already_called:=false;
        glob:=a_class.create;
        container_array[255]:=a_container.create;
        container_array[255].a_field:=glob;
        glob.a_method;
        if error then
          halt(1);
end.
