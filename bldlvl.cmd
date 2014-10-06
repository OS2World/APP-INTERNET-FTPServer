/*----------------------------------------------------------
   Appends a build level to FTPD.EXE and SETUP.EXE.

           Author:       Peter Moylan
           Last revised: 10 July 2011

   Usage:
           bldlvl ver

           where ver is the version string

------------------------------------------------------------*/

parse arg ver
projHost = "PJM2"
timestamp = LEFT(DATE() TIME(),25)LEFT(projHost,10)
signature0 = "@#Peter Moylan:"ver"#@##1## "timestamp"::EN:AU:::@@"
outfile = "level.txt"
"@DEL "outfile" 2> nul"
CALL LINEOUT outfile, signature0||"FtpServer FTP daemon for OS/2 and eCS"
CALL STREAM outfile,'C','CLOSE'
"@copy ftpd.exe /B + level.txt ftpd.exe /B > nul"

"@DEL "outfile
CALL LINEOUT outfile, signature0||"Configuration of FtpServer daemon"
CALL STREAM outfile,'C','CLOSE'
"@copy setup.exe /B + level.txt setup.exe /B > nul"
"@DEL "outfile

exit

