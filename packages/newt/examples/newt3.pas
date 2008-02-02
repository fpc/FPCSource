program newt3;

uses newt;


var

    form, labelC, entry, button : newtComponent;
    S: ansiString;
    P: Pchar;
    machaine :string;
begin

    newtInit();
    newtCls();

    newtOpenWindow(10, 5, 40, 8, 'Entry and Label Sample');

    labelC := newtLabel(1, 1, 'Enter a string');

    p:=addr(machaine[1]);
    s:='Sample';
    entry := newtEntry(16,1,PChar(S),20,@P,NEWT_FLAG_SCROLL OR NEWT_FLAG_RETURNEXIT);


    button := newtButton(17, 3, 'Ok');
    form := newtForm(NiL, NiL, 0);
    newtFormAddComponents(form, labelC, entry, button, NiL);

    newtRunForm(form);

    newtFinished();

    writeln('Final string was:'+ strPas(P));

    (* We cannot destroy the form until after we've used the value
       from the entry widget. *)
    newtFormDestroy(form);
end.
