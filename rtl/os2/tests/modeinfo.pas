program modeinfo;

type    viomodeinfo=record
            cb:word;                         { length of the entire data structure }
            fbType,                          { bit mask of mode being set }
            color: byte;                     { number of colors (power of 2) }
            col,                             { number of text columns }
            row,                             { number of text rows }
            hres,                            { horizontal resolution }
            vres: word;                      { vertical resolution }
            fmt_ID,                          { attribute format }
            attrib: byte;                    { number of attributes }
            buf_addr,
            buf_length,
            full_length,
            partial_length:longint;
            ext_data_addr:pointer;
        end;
        Pviomodeinfo=^viomodeinfo;

function _VioGetMode (var Amodeinfo:viomodeinfo;viohandle:word):word;[C];
function _VioSetMode (var Amodeinfo:viomodeinfo;viohandle:word):word;[C];

var mode:viomodeinfo;

begin
    mode.cb:=sizeof(mode);
    writeln('getmode= ',_viogetmode(mode,0));
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
