program testcgi;

uses uncgi;

begin
  cgi_init;
  set_content('text/html');
  Writeln ('User agent = ',http_useragent);
  Writeln ('Referer    = ',http_referer);
  Writeln ('Name       = ',get_value('name'));
  Writeln ('Address    = ',get_value('address'));
  Writeln ('City       = ',get_value('city'));
end.
{
}
