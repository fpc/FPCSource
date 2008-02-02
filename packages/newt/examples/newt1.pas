program newt1;


(* a newt basic sample program *)

uses newt;


begin
    newtInit();
    newtCls();

    newtDrawRootText(0, 0,'Some root text');
    newtDrawRootText(0, 1,'Press a key to display a help line   ');

    newtPushHelpLine(nil);

    newtRefresh();

    newtWaitForKey;

    newtPushHelpLine('A help line');

    newtDrawRootText(0, 1,'Press a key to hide the help line    ');
    newtRefresh();
    newtWaitForKey;

    newtPopHelpLine();
    newtRefresh();
    newtDrawRootText(0, 1,'Press a key to exit                  ');
    newtWaitForKey;

    newtFinished();
end.
