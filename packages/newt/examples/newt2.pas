program newt2;

uses newt;



var

    form, b1, b2 : newtComponent;

begin

    newtInit();
    newtCls();

    newtOpenWindow(10, 5, 40, 6, 'Button Sample');

    b1 := newtButton(10, 1, 'Ok');
    b2 := newtCompactButton(22, 2, 'Cancel');
    form := newtForm(Nil, Nil, 0);
    newtFormAddComponents(form, b1, b2, Nil);

    newtRunForm(form);

    newtFormDestroy(form);
    newtFinished();
end.
