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
end.  Revision 1.2  2002-09-07 15:43:06  peter
end.    * old logs removed and tabs fixed
end.
end.  Revision 1.1  2002/01/29 17:55:23  peter
end.    * splitted to base and extra
end.
}
