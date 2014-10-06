/*----------------------------------------------------------
   Returns the version number of FtpServer.

           Author:       Peter Moylan
           Last revised: 10 July 2011

   Usage:
           ver = version()

           (Run this from the FtpServer top-level directory)

------------------------------------------------------------*/

DEFFile = "DEF\FV.def"

DO FOREVER
    IF lines(DEFFile) != 1 THEN LEAVE
    parse value linein(DEFFile) with kwd'='val
    kwd = STRIP(kwd)
    IF kwd = "version" THEN LEAVE
END

/* Extra the part of val inside double quotes. */

PARSE VALUE val WITH v1 '"' version '"' v2
RETURN version

exit

