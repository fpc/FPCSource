var searchWindow;

self.name = "FPCMainHelp";

function reloadfile()
	{
	if (searchWindow.searchOpened)
		;// don't do anything other than open the window.
	else
		searchWindow.location.replace("../search/search.html");
	searchWindow.focus();
	}

function opensearch()
	{
	if (parseInt(navigator.appVersion.charAt(0)) >= 4)
		{
		if (navigator.appName == "Netscape")
			{
			searchWindow = window.open("","searchwin","innerWidth=320,innerHeight=380,status=no");
			reloadfile();
			}
		else if (navigator.appName.indexOf("Microsoft") != -1)
			{
			searchWindow = window.open("","searchwin","Width=320,Height=380,status=no");
			reloadfile();
			}
		}
	}
