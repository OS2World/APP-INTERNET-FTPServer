/* Batch file to create the FtpServer distribution. */
/* The result is one distribution zip file and one source zip file. */

'del ftpser*.zip 2>nul'
'del temp /N 2> nul'
'call deltree /Y temp >nul'           /* deltree3.zip from Hobbes */
cd doc
'ipfc -i ftpserver.ipf'
cd ..
'cd ..\FSU'
'xc =p setup.prj'
'cd \dev1\Monitor'
'xc =p monitor.prj'
'cd \MyApps2\ftpser'
'copy ..\FSU\Setup.exe'
'copy \dev1\Monitor\Monitor.exe'
'xc =p ftpd.prj'
'xc =p viosetup.prj'
'xc =p Tmonitor.prj'
'xc =p loadprm.prj'
'xc =p storeprm.prj'
'xc =p loganaly.prj'
'\apps\lxlite\lxlite *.exe'
ver = version()

/* Generate symbol files.  If you don't have Perl,   */
/* you can omit the next three lines.                */

'call PerlEnv.cmd'
perl 'D:\Apps\scripts\makexqs.pl' ftpd.map
say "ftpd.sym and ftpd.xqs should now exist"

/* Set some version numbers and icons. */

call bldlvl ver
call seticon

/* Put all the files we want to zip up into a directory 'temp'. */

'copy D:\Dev1\general\doc\gpl.txt'
mkdir temp
cd temp
mkdir doc
'copy ..\doc\changes.txt doc'
mkdir tools
'copy ..\tools\README tools'
'copy ..\tools\AddUser.cmd tools'
'copy ..\tools\CloseServer.cmd tools'
'copy ..\tools\FindPassword.cmd tools'
'copy ..\tools\ftpdctl.cmd tools'
'copy ..\tools\GetClientAddrs.zip tools'
'copy ..\tools\LastLogin.cmd tools'
'copy ..\tools\LimitedUse.cmd tools'
'copy ..\tools\MakeLetter.cmd tools'
'copy ..\tools\mdumpini.cmd tools'
'copy ..\tools\mloadini.cmd tools'
'copy ..\tools\Migrate.cmd tools'
'copy ..\tools\MoveLog.cmd tools'
'copy ..\tools\NewfwIP.cmd tools'
'copy ..\tools\OneTime.cmd tools'
'copy ..\tools\remove.cmd tools'
'copy ..\tools\ResetPos.cmd tools'
'copy ..\tools\ShutFtpd.cmd tools'
'copy ..\README'
'copy ..\seticon.cmd'
'copy ..\gpl.txt'
'copy ..\file_id.diz'
'copy ..\doc\ftpserver.inf doc'
'copy ..\ftpd.exe'
'copy ..\ftpd.map'
'copy ..\ftpd.sym'
'copy ..\ftpd.xqs'
'copy ..\setup.exe'
'copy ..\viosetup.exe'
'copy ..\monitor.exe'
'copy ..\tmonitor.exe'
'copy ..\loadprm.exe'
'copy ..\storeprm.exe'
'copy ..\ftpd.fmt'
'copy ..\setup.fmt'
'copy ..\monitor.fmt'
'copy ..\QuickStart.cmd'
'copy ..\loganaly.exe LogAnalysis.exe'
'copy ..\folder.cmd'
'copy ..\doc\anonymous.prm doc'
'copy ..\doc\example.prm doc'
'copy ..\doc\manager.prm doc'
'copy ..\doc\welcome.msg doc'

/* Zip up the main package. */

'zip -q -r ..\ftpser_'ver'.zip .'
'del doc\* /n'
rmdir doc
'del tools\* /n'
rmdir tools
'del * /n'
cd ..

/* SOURCE FILES */

/* Sources for the server and some utilities. */

'del src1.zip /N 2>nul'
'imports ftpd | zip -q -j src1.zip -@'
'imports viosetup | zip -q -j -u src1.zip -@'
'imports tmonitor | zip -q -j -u src1.zip -@'
'imports loadprm | zip -q -j -u src1.zip -@'
'imports storeprm | zip -q -j -u src1.zip -@'
'imports loganaly | zip -q -j -u src1.zip -@'
'zip -q src1.zip ftpd.prj viosetup.prj tmonitor.prj loadprm.prj storeprm.prj loganaly.prj'

/* The Setup sources. */

'del src2.zip /N 2>nul'
'Imports ..\FSU\Setup | zip -q -j src2.zip -@'
'zip -q -j src2.zip ..\FSU\setup.prj ..\FSU\res\DID.res'

/* The Monitor sources. */

'del src3.zip /N 2>nul'
'Imports \Dev1\Monitor\Monitor | zip -q -j src3.zip -@'
'zip -q -j src3.zip D:\Dev1\Monitor\monitor.prj D:\Dev1\Monitor\RES\Monitor.res'

/* Move all source files into the temp directory, unzip and re-zip.  */

'cd temp'
'move ..\gpl.txt .'
'copy ..\BUILDING'
'copy ..\makezip.cmd'
'copy ..\version.cmd'
'copy ..\bldlvl.cmd'

'mkdir FtpServer'
'move ..\src1.zip FtpServer'
'cd FtpServer'
'unzip -q -o src1.zip'
'del src1.zip /N'
'mkdir doc'
'copy ..\..\doc\ftpserver.ipf doc'
'cd ..'

'mkdir Setup'
'move ..\src2.zip Setup'
'cd Setup'
'unzip -q -o src2.zip'
'del src2.zip /N'
'cd ..'

'mkdir Monitor'
'move ..\src3.zip Monitor'
'cd Monitor'
'unzip -q -o src3.zip'
'del src3.zip /N'
'cd ..'

'del ..\ftpserSrc_'ver'.zip /N 2>nul'
'zip -q -r ..\ftpserSrc_'ver'.zip .'

/* Remove temporary files. */

cd FtpServer
cd doc
'del * /N'
'cd ..'
'rmdir doc'
'cd ..'
'del FtpServer\* /N'
'rmdir FtpServer'
'del Setup\* /N'
'rmdir Setup'
'del Monitor\* /N'
'rmdir Monitor'
'del * /N'
'cd ..'
rmdir temp

