program modeinfo;

uses viocalls;

var mode:Tviomodeinfo;

begin
    mode.cb:=sizeof(mode);
    writeln('getmode= ',viogetmode(mode,0));
    writeln('cb= ',mode.cb);
    writeln('fbtype= ',mode.fbtype);
    writeln('color= ',mode.color);
    writeln('col= ',mode.col);
    writeln('row= ',mode.row);
    writeln('hres= ',mode.hres);
    writeln('vres= ',mode.vres);
    writeln('fmt_ID= ',mode.fmt_ID);
    writeln('attrib= ',mode.attrib);
    writeln('buf_addr= ',mode.buf_addr);
    writeln('buf_length= ',mode.buf_length);
    writeln('full_length= ',mode.full_length);
    writeln('partial_length= ',mode.partial_length);
end.
