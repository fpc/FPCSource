pmdemo1.exe: pmdemo1.pp pmdemo1.res
	ppc -Dow -Ch8096 -Cs8096 pmdemo1.pp

pmdemo1.res: pmdemo1.rc
	rc -r pmdemo1
