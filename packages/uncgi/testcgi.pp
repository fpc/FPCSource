program testcgi;

uses uncgi;

begin
  cgi_init;
  Writeln ('User agent = ',http_useragent);
  Writeln ('Referer    = ',http_referer);
  Writeln ('Name       = ',get_value('name'));
  Writeln ('Address    = ',get_value('address'));
  Writeln ('City       = ',get_value('city'));
end.  $Log$
end.  Revision 1.2  2000-07-13 11:33:32  michael
end.  + removed logs
end. 
}
