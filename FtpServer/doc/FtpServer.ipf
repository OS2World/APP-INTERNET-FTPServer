:userdoc.
:title.FtpServer documentation
:docprof toc=123.

.***********************************
.*   INTRODUCTION
.***********************************

:h1 res=1000 id=1000 global.Introduction
:p.
FtpServer is an ftp daemon for OS/2. Formerly shareware, it
is now distributed as an open-source program under the GPL licence.
This documentation is for version 2.8.
:p.
:hp2.Disclaimer of Warranty:ehp2.

:sl compact.
:li.
:hp8.
This Product is provided "as-is", without warranty of any
kind, either expressed or implied, including, but not limited to,
the implied warranties of merchantability and fitness for a
particular purpose. The entire risk as to the quality and
performance of the Product is with you. Should the Product prove
defective, the full cost of repair, servicing, or correction lies
with you.
:ehp8.
:esl.

:p.
The author of FtpServer is Peter Moylan, peter@pmoylan.org.

:p.
The latest version of FtpServer is normally kept at ftp&colon.&slash.&slash.ftp.pmoylan.org/FtpServer.
.br
Information about other software on this site may be found at
http&colon.&slash.&slash.www.pmoylan.org/pages/os2/software.html.

:p.
You may, if you wish, join a mailing list for discussions and questions
about FtpServer. To join the list, send an
e-mail to majormajor@os2voice.org. The subject line is not
important and may be anything. In the body of the message, put the
lines
:xmp.       subscribe ftpserver-list
       end
:exmp.

:p.To have yourself removed from the list, send a similar e-mail but
using the command "unsubscribe" instead of "subscribe".

.***********************************
.*   PREREQUISITES
.***********************************

:h1 id=prerequisites.Prerequisites

:hp2.Prerequisites:ehp2.

:p.This software assumes that both INIDATA.DLL and XDS230M.DLL are in your
LIBPATH. If, when trying to run FTPD.exe or Setup.exe, you get a message like
"The system cannot find the file XDS230M", you must install INIData,
version 1.0 or later. (With older versions of OS/2, it needs to be version 1.1
or later.) INIData can be found at the same web or FTP site as where
you found the FtpServer zip file.

.***********************************
.*   OBTAINING THE SOURCE CODE
.***********************************

:h1 id=sourcecode.Obtaining the source code

:hp2.Obtaining the source code:ehp2.

:p.The sources for this package should be obtainable from the same place
that you found this software.  Look for a file called ftpserSrc_N.N.zip,
where N.N is the version number.

.*************************************
.*   DOCUMENTATION IN OTHER LANGUAGES
.*************************************

:h1.Documentation in other languages
:hp2.Documentation in other languages:ehp2.
:p.
To simplify the distribution, the FtpServer zip file normally contains only
English-language documentation. To get documentation in another language,
go to ftp&colon.&slash.&slash.ftp.pmoylan.org/inf and look for
a file called ftpserver.inf.xxx, where xxx indicates the language.
If you want to volunteer to do another translation, you will find the original
document source (FtpServer.IPF) in the DOC directory of this distribution.

:p.The copyright on the translations belongs to the people who did
the translations.

:p.The versions that are now available are
:table cols='19 19 23'.
:row.
:c.Italiano/Italian
:c.ftpserver.inf.it
:c.Andrea Brancatelli
:row.
:c.Pyccku&ug./Russian
:c.ftpserver.inf.866
:c.Konstantin Boyandin
:row.
:c.Svenska/Swedish
:c.ftpserver.inf.046
:c.Bj”rn S”derstr”m
:row.
:c.Svenska/Swedish
:c.ftpserver.html.046
:c.Bj”rn S”derstr”m
:etable.

.***********************************
.*   SERVER FEATURES
.***********************************

:h1 res=002.Server features

FtpServer is an ftp server program that implements essentially all of the ftp standard
RFC 959, as updated by RFC 1123. It supports re-get and passive mode transfers. The system manager
can control which directories are visible to users, and the kind of
access (read, write, delete, rename) allowed in each directory. Files bigger than
2 GiB are handled correctly.

:p.
For further details, see
:ul compact.
:li.:link reftype=hd refid=special.What's special about FtpServer:elink.
:li.:link reftype=hd refid=limitations.Limitations and missing features:elink.
:li.:link reftype=hd refid=quirks.Quirks:elink.
:li.:link reftype=hd refid=troublesome.Troublesome clients:elink.
:li.:link reftype=hd refid=nonstandard.Nonstandard features:elink.
:eul.

:h2 id=special.What's special about FtpServer?

:ul.
:li.Supports essentially all of what is in the FTP standard, including re-get and
passive mode. The only missing features are those that are generally considered
to be obsolete. (For example, support for the PDP-10 file system.)

:li.Fast and compact.

:li.Compatible with all FTP clients that I've been able to test. If an FTP
client is going to fail with this server, it is probably going to fail in
the same way with other FTP servers.

:li.Support for code pages 437, 850, 866, 1208, and 1251. (Others can be added
on request.)

:li.Separate read, write, delete, and rename permission for each directory
the user can see. You can also make directories invisible.
Users can be given access to multiple drives or network drives, if desired.

:li.The directories seen by a user can include symbolic links.

:li.The file structure seen by a user is configurable on a per-user
basis. Different users see different directory trees. Nobody can see
how this relates to the "real" drive and directory structure, unless
you set up a "manager" account and specify that it is allowed to see
every drive.

:li.You can restrict the IP addresses from which clients can log in,
you can restrict the number of simultaneous logins from the
same address, and you can specify per-user speed limits.

:li.Can be run from inetd.

:li.Can be run detached.

:li.Can be configured to run behind a firewall.

:li.Several different logging options.

:li.Includes a check for "pirate" operations&colon. if a pirate upload
is detected, the upload is allowed to continue at a reduced speed, and
then all uploaded files are deleted at the end of the session.

:li.An option that permits remote configuration.

:li.The optional Monitor utility allows you to see who is logged in,
and also allows you to kill client sessions. This can be run either
locally or remotely. (A password for a manager account is required.)

:eul.

.**************************************
.*   LIMITATIONS AND MISSING FEATURES
.**************************************

:h2 id=limitations.Limitations and missing features
:hp2.Limitations and missing features:ehp2.

:p.
These are things I might fix up when I have the time, but they have
very low priority because they relate to features that are generally
considered to be obsolete.

:ul.

:li. Transfer types: only Ascii, Image, and "Local 8" are supported.
Support for Fortran carriage control and EBCDIC will probably
never be added, since those features are needed mainly when
performing transfers to/from machines that became obsolete long
before the invention of the microprocessor.

:li. Page-structured files will probably never be supported.
(As far as I know, only PDP-10 systems support this feature.)

:li. The only supported transmission mode is stream mode. I might
or might not add support for block mode and compressed mode
at a later stage. For the moment, there doesn't seem to be
any demand for these extras - I haven't come across any ftp
client that uses them.

:eul.

:p.Every command in the standard RFC 959 is implemented.

.***********************************
.*   QUIRKS
.***********************************

:h2 id=quirks.Quirks
:hp2.Quirks:ehp2.

:p.
These might be seen as errors, but there's no need to fix them
because they don't have a harmful effect.

:ul.
:li. Can "change directory" to a directory that doesn't exist, if that directory is
shown as visible in the user's permission data. Not a real
problem, because the user sees an empty listing and can't do
any operations in that directory.

:eul.

.***********************************
.*   TROUBLESOME CLIENTS
.***********************************

:h2 id=troublesome.Troublesome clients
:hp2.Troublesome clients:ehp2.

:p.
Different ftp clients work differently, and some of them don't
bother to adhere to the standards, so there will probably always
be some client incompatibilities. The ones I know about so
far are:

:ol.
:li.One of the MS-Windows ftp clients (I've forgotten
which one) gives unreasonable time delays when listing a short
directory, although long listings are quite fast. The tests
I have been able to do suggest that this problem occurs only
when the client and server machines are physically close to
each other. Once the network delays rise to more typical
values, the problem goes away.

:li.I have been told of a problem when using ws-ftp, involving a
"can't change directory" symptom when fetching the entire
contents of a directory; but I've been unable to
reproduce the problem and I'm still not quite sure of the
precise nature of the problem. I suspect that ws-ftp has
by now been updated to eliminate this bug.

:li.Apparently some proxy servers can't handle multiline responses
to FTP commands. If you hit this problem, you might be able to
solve it by deleting the file WELCOME.MSG. The problem occurs
mainly in servers developed prior to 1985, so it is not likely to
occur with modern non-Microsoft servers. There is a continuing problem
with servers whose code was copied from really ancient Unix
software, but for the most part that affects only users in a
Windows-only network.

:li.EmTec FTP gets itself confused when doing uploads&colon. it seems
to lose track of which responses belong to which commands, apparently
because of a buggy implementation of the STOR command. (The problem
will occur with any server, not only with FtpServer.) Since the
EmTec website no longer mentions this product, it seems unlikely that
the bug will ever be fixed.

:li.FTP Browser is a "greedy" client that attempts to improve speed
by opening multiple parallel sessions. This should work (although
it is not very considerate to other clients of the same server), but
in fact FTP Browser seems to be unable to handle the case where it
runs over the limit on the number of connections that the server
will accept. You can fix this by increasing the limits, but it would
probably be a better idea to suggest that your users switch to an
FTP client where the bugs are more likely to be fixed.

:li.I have been told of problems with NetDrive, but I believe that
those have now been fixed.

:eol.

:p.This is probably leaving you wondering which ftp clients do work.
In fact most clients, especially the command-line clients,
work quite satisfactorily for transferring one
file at a time. When difficulties arise, it is usually during advanced
operations such as transferring an entire directory tree. The ftp
client that I use most of the time is NcFtp, and that is reliable
for everything except doing a "resume" on a failed upload.

.***********************************
.*   NON-STANDARD FEATURES
.***********************************

:h2 id=nonstandard.Non-standard features
:hp2.Nonstandard features:ehp2.

:p.
The program violates RFC959 in the following ways:

:ol.
:li. Extra commands SIZE and MDTM are implemented.

:li. The FEAT and OPTS commands, as described in RFC2389,
have been added.

:li. The EPRT and EPSV commands, as described in RFC2428,
have been added.

:li. A :link reftype=hd refid=multidomain.HOST:elink.
command based on RFC7151 has been added.

:li. The SYST command returns a reply of UNIX rather than OS/2.
I had to do this because the "correct" reply causes
WebExplorer to misinterpret the directory listings, and
I'm told that at least one Microsoft ftp client will refuse
to connect to a server that identifies itself as OS/2.

:li. The obsolete and non-official commands XMKD, XRMD, XPWD, XCUP,
and XCWD are implemented. (These have been obsolete since 1985,
and most ftp clients don't use them; but apparently they are still
needed when connecting with some Windows software.)

:eol.

.***********************************
.*   INSTALLATION
.***********************************

:h1.Installation
:hp2.Installation:ehp2.
:p.
See also :link reftype=hd refid=deinstall.De-installation:elink.

:p.
You should have received this package in the form of a zip file.
To install it, simply unzip the file into a directory of your choice.
(Presumably you have already done this.) If you are upgrading from an
earlier version, you can unzip into the same directory as the old
version, and the existing user information will be retained. The
server is now ready to run.

:p.
The server itself is the program called ftpd.exe. You can run it
either by double-clicking on the desktop icon, or by entering the
command "ftpd" in a command-line session. (In the latter case, make
sure you are in the right directory, otherwise you'll end up running
the ftpd that was supplied with OS/2.) Most people will want to
put a program object or shadow for ftpd into the startup folder,
or start the server from \TCPIP\BIN\TCPEXIT.CMD,
so that the server will run each time the system is booted; but that's
up to you.

:p.
Even though the server will work "out of the box", you still need to
define the user permissions so that clients can connect to the server.
You can do this either before running the server, or while it is
running. See :link reftype=hd refid=setupusers.Setting up the User
Permissions:elink..

:p.
For some other options, see
:ul compact.
:li.:link reftype=hd refid=parameters.Command line parameters:elink.
:li.:link reftype=hd refid=inetd.Running from inetd:elink.
:li.:link reftype=hd refid=detached.Running FtpServer detached:elink.
:li.:link reftype=hd refid=welcome.Welcome messages:elink.
:eul.

.***********************************
.*   DEINSTALLATION
.***********************************

:h1 id=deinstall.De-installation
:hp2.De-installation:ehp2.
:p.
FtpServer does not tamper with CONFIG.SYS or with other system files.
If you decide that you don't want to keep FtpServer, simply delete
the directory into which you installed it.

.***********************************
.*   UNDERSTANDING PORT NUMBERS
.***********************************

:h1 id=UnderstandingPorts.Understanding port numbers
:hp2.Understanding port numbers:ehp2.

:p.Each node in a network is identified by an IP address, which is a
32-bit number. (The IP version 6 protocol turns this into a
128-bit number, but version 6 is not yet supported by the OS/2
version of tcp/ip.) In the simplest case, a computer has just one
IP address, but it is possible - and becoming more common - for
a computer to have more than one network interface, and
therefore more than one IP address.

:p.The IP addresses of the form
:ul.
:li.(Class A) 10.*.*.*
:li.(Class B) 172.16.*.* through 172.31.*.*
:li.(Class C) 192.168.*.*
:eul.
:p.are special, in that they are private to the local area network (LAN)
in which they are used. For most LANs the Class C addresses would
be the most appropriate, but the Class A and Class B ranges are
available for very large LANs. The advantage of these addresses is that
you can assign them to nodes in a LAN without having to go through
the official process of having an IP address allocated to you. The
disadvantage is that these addresses are invisible outside the LAN;
that is, these nodes cannot communicate with the global Internet. The
way around this disadvantage is to use a firewall or router that implements
Network Address Translation (NAT). NAT will be discussed in a later
section.

:p.At any given time, a network node might have several connections
in progress with various machines around the network. This means
that we need a way of labelling the different connections, so that we
can tell them apart. The method used is to assign port numbers to
(our end of) the connections. The port number has nothing to do with
hardware I/O ports. Instead, it is simply a numbering system to
label the connections. A port number is an unsigned 16-bit number.
One end of a connection is uniquely identified by the pair
(IP address, port number). A connection is defined by two of these
pairs, one for each end.

:p.The software mechanism for setting this up is known as a "socket".
You can think of the socket as a data structure that keeps track
of the two IP addresses and two port numbers involved in the
transfer, together with whatever other state information is needed.
(Data buffers, byte counters, etc.)

:p.When a socket is first created, we know only the IP address and
port number at our own end. Once a connection is established, we
get to find out the IP address and port number at the other end.
Naturally, one of the two ends has to be the one to initiate the
connection. The usual mechanism is that the server goes into a
"listening" state where it waits for client connections, and then
the client end actually establishes the connection.

:p.Port numbers in the range 0 to 49151 are reserved for "server" ports.
More precisely, those in the range 0 to 1023 are defined by official
standards. Those in the range 1024 to 49151 have a less official status,
but they are still considered to be "registered ports" which are
allocated to known applications. A (slightly out of date) list of these reserved port numbers
can be found in the file &bslash.MPTN&bslash.ETC&bslash.SERVICES.

:p.Port numbers in the range 49152 to 65535 form a pool of "available"
ports which are used whenever a new port is needed. Typically one
of these ports is allocated for a short-term connection, and then
deallocated once the operation is done.

:p.In a client/server protocol, the clients need to have some way of
finding the servers. For this reason, servers always listen on
"well known ports" which are reserved for this purpose. The FTP
protocol uses two connection channels, one for commands and one
for data. Consequently it has two well known ports: port 21 for
the commands, and port 20 for the data. This, of course, is at the
server end. At the client end, the client can use whatever ports
it wants, and normally the client will choose its ports from the
pool of available ports.

:p.The FTP protocol allows for two kinds of data connection. In
so-called "passive ftp" the data transfer is initiated from the
client end. In non-passive ftp, also known as "port ftp", the
transfer is initiated from the server end. In both cases the command
connection uses port 21 at the server end. The difference between
the two methods lies in the way the data ports are allocated.


:p.:hp2.Passive FTP:ehp2.

:p.Passive FTP is initiated when the client issues a PASV command.
(The effect of this lasts for just one file transfer. If there is a
second file to be transferred, the client has to issue a second
PASV command.) The server responds to this command with a line like

:xmp.

   227 Entering passive mode (127,0,0,1,203,197)
:exmp.

:p.The first four of those numbers specifies the IP address of the
server. The remaining two specify a port number. In effect the
PASV command is saying "please choose a data port, and tell me
what port it is". The server chooses the port, normally from the
big pool of available port numbers, and reports its number back to
the client. The server then listens at that port for the client
to initiate a data connection. Of course the client must also
choose a data port at its own end; the server finds out which port
it is after the data connection is established.

:p.In passive ftp, the command channel and the data channel behave
in the same way. The server listens at a known port. The client
knows what the port number is, so it can initiate a connection to
that port.


:p.:hp2.Port (non-passive) FTP:ehp2.

:p.The earliest drafts of the ftp standards did not have any passive
ftp. The "normal" way to transfer data, and even now the most
common way, is to have the
data connection established from the server end. (Exception: most
web browsers, as distinct from standalone ftp clients, seem to
use passive ftp exclusively. The standalone ftp clients usually
give the user the choice.) For port ftp to work,
the server has to know which port the client is listening on.

:p.This is specified with a PORT command from the client. An
example of this command is

:xmp.

   PORT 127,0,0,1,203,201
:exmp.

:p.This specifies the IP address of the client, and the port number
that the client wishes to use for the data transfer. That is, the
client chooses a port number, it listens on that port, and the
server makes a connection to that port. In both this and the
PASV example, the IP address was 127.0.0.1. That's because I used
the loopback connection on my computer to generate the examples.
In practice, the IP address will be whatever address belongs to
your own machine.

:p.Meanwhile, the server must also choose a port number at its own end.
By standard convention, this is almost always port 20.

:p.Note that the client must always give either a PASV command or
a PORT command before it starts an upload or download. Which of
these it gives controls whether we use a passive or non-passive
transfer.

:p.:hp2.Security considerations:ehp2.

:p.The PORT command includes an IP address. This would
normally be the IP address of the client, but the FTP protocol allows
it to be an arbitrary address. This allows for &odq.three-cornered&cdq.
transfers, where a client logs into two different servers and then
specifies a file transfer between those two machines.

:p.Although this "three-cornered" facility can be convenient for
system managers, it also creates a security hazard. A malicious user
could log into your server, send a PORT command that addresses a
different server, and then attempt to break in to that different
server. The system logs will show that the attack is coming from
your machine, so you could be unjustly blamed for the attack.

:p.To guard against this problem, FtpServer imposes the following
rules&colon.
:ul.
:li.A port number less than 1024 will always be rejected. Those
port numbers belong to the well-known servers, so they should never
be used as data ports. (Except in the special case where the server
chooses to use port 20; but that is an internal decision inside the
server, not a request from the client.)
:li.A user who is logged on as a guest user - that is, someone who
is allowed to supply an e-mail address as a password - is not allowed
to do a three-cornered transfer. The address in the PORT command
must match the user's address.
:eul.

:p.:hp2.The effect of a firewall or router:ehp2.

:p.A firewall or router might or might not include the NAT option. In this
section, I will ignore NAT.

:p.The function of a firewall is to allow some packets to pass
through, while refusing to let others pass through. This is
done by a set of rules created by the system administrator. The
administrator has to decide which classes of traffic are legal.

:p.The function of a router is to allow several machines to be connected
in a network, by looking at the packets that arrive at the router and
sending them to the right destination address. Many routers also have
a firewall option, so that one physical device does both jobs.

:p.I don't have much experience with firewalls, so I can't give an
expert description of what happens here. I believe, however,
that the rules are usually based on port numbers. Traffic to/from
certain ports is allowed, while other ports are blocked. The
rules would normally have to be asymmetric, in the sense that
the rules for outgoing packets would be quite different from the
rules for incoming packets.

:p.Consider the case where an ftp client is behind a firewall, and
is talking to a server that is not behind a firewall. A
typical choice of firewall rules would make non-passive ftp
illegal, because non-passive ftp requires the server - the machine
outside the firewall - to initiate the data connection. A
firewall is often set up in such a way that machines outside
the firewall are not allowed to initiate a connection. In fact,
this is a very large part of the motivation for adding passive ftp
to the ftp standard. If the client is behind a firewall, then
normally it should use only passive ftp.

:p.Conversely, if the server is behind the firewall and the client is
not, then passive ftp is likely to be blocked and port ftp is the
only sensible option.

:p.If the client is behind a firewall, and the server is behind a
different firewall, then we are in trouble. The 'firewall'
concept was not designed with this situation in mind. Traditionally servers
were not supposed to be behind a firewall, except of course for servers
that are supposed to be private to the LAN. If you had a firewall
protecting your LAN, then you would normally configure your public server
applications such that they are technically outside the firewall.
Often the easiest way to do this is to put the server software on the
same machine that is running the firewall software. If you
want radically different rules for 'internal' and 'external' users,
the least complicated solution is to install two copies of FtpServer, at
two different IP addresses.

:p.If, for any reason, you really have to have your server behind a
firewall, then you had better be an expert in designing the
firewall rules. You should read the preceding sections very
carefully, to see which ports should be enabled in the firewall
rules. Actually, that part is easy. The difficult part is to
do this in such a way that you allow the ftp server to function,
but without compromising the security of your LAN. If you make
the rules too permissive, you might as well not have a firewall.

:p.A more modern solution is to keep your server inside the LAN, but use
NAT to allow connections from outside. That option is covered in the
following section.

:p.:hp2.Firewalls and routers implementing NAT:ehp2.

:p.The best sort of firewall or router is one that offers the option of
Network Address Translation (NAT). This is a feature that allows
your machines inside the LAN to have their own IP addresses, but
which lets them appear to have a different IP address as seen
by the outside world. The most typical case is where all the machines in
your LAN share the same "external" IP address.

:p.Sometimes the NAT function is implemented in the firewall, and
sometimes it is in the router. Often, as noted in the previous section,
the same physical hardware is used to do both. One very common
situation is where all of your local machines are connected to a
router that does NAT, and the router is in turn connected to an ADSL or cable modem
that does NAT. Normally this is a trouble-free connection, but if you
have a complicated network you might have to think hard about the implications
of doing NAT twice.

:p.An IP data packet has a header that specifies, among other things,
a source and destination IP address. That says who is sending the
packet, and who is supposed to receive it. With the NAT feature
in place, the NAT hardware alters the source IP address, to make it
appear as if the packet came from a different address. That is for
outgoing traffic. For incoming traffic, the NAT hardware intercepts
the packets destined for the "fake" IP address, and sends them
to the real intended recipient.

:p.Remark&colon. To configure your router to implement this function, look
at the web interface for your modem or router, and look for a feature
called "forwarding" or "port mapping". In that section, look for rules
for "FTP" or "port 21". (You might have to invoke an option to add a
new rule.) Then specify that traffic for that port should be sent to
the address in your LAN where FtpServer is running.

:p.Consider the case where you have an ftp client behind a firewall,
and an ftp server outside the firewall. When the client connects
to the server, the server does not see the client's true IP address.
Instead, it sees the address of the firewall. The server doesn't
even know that a firewall is present. It simply interacts with
that address, just as it would with any client. As far as the
server is concerned, the client is the firewall machine. However,
the NAT routing passes on the server's responses to the true client.

:p.Similarly, if a server is behind a firewall then every client
outside the firewall thinks that the server is at the same
address as the firewall machine. The firewall modifies the
addresses, and passes the traffic on to the true address
of the server.

:p.All of this would work well except for two little details. As we
have seen in earlier examples, the PASV and PORT commands of the
ftp protocol send IP addresses as data. These are the true IP
addresses, not the addresses as altered by the NAT software.
This can result in data being sent to the wrong address.

:p.The logical solution to this problem would be for the NAT device
to intercept the PASV and PORT commands, and alter the numbers
in those lines. Some implementations of NAT are smart enough to do this.
Unfortunately, some others are not able to make this adjustment.

:p.In the past, people didn't normally think in terms of putting an
ftp server behind a firewall. Now that we have cable modems,
ADSL, and various other ways to get high-speed data links, that
option is becoming more common. To deal with this complication,
FtpServer has a 'behind firewall' option to compensate for what
the NAT software should be doing, but isn't doing. See the
:link reftype=hd refid=PMConfigUser1a.Options:elink. page of this
manual for details of how to set it up. Note that this option will
not be necessary if you have a sufficiently smart firewall or router.

:p.When this option is enabled, the server has to know the
difference between clients that are outside the firewall, and those
that are inside the same firewall as the server. For this
reason, the Setup notebook also lets you specify a range of IP
addresses - presumably the addresses that belong to your LAN -
for which the 'behind firewall' rules should not be applied.
This can be found on the
:link reftype=hd refid=LocalAddrs.Local Addresses:elink.
page of the Setup notebook.

.***********************************
.*   FILE NAMES
.***********************************

:h1 id=FileNames.File names
:hp2.File names:ehp2.

:p.The original FTP standards assumed 7-bit ASCII file names, but this is
unsatisfactory for many languages. The extension proposal RFC2640 solves
this by requiring that filenames in FTP commands and responses be
transferred between client and server in the UTF-8 character set. (UTF-8 is an implementation of
Unicode.) FtpServer conforms to this standard.

:p.The OS/2 file system does not, however, use any form of Unicode.
It uses "code pages", where each code page defines a set of 256
characters. The first 128 characters in a code page are for ASCII
characters. The remaining 128 characters are different from one
code page to the next. You choose whichever code page gives the best
support for your language.

:p.FtpServer handles this by translating back and forth between UTF-8
and the current code page. This works well if the server and client
machines have compatible code pages. Working across countries can
still be troublesome, because you can meet characters that have a
UTF-8 code but that don't exist in the current code page. We do not
have a good solution for that situation, but FTP clients that understand
UTF-8 should be able to handle it. All they have to do is send back
exactly the form of a file name that they saw in a directory listing.

:p.At present FtpServer has full support for code pages 437, 850,
866, 1208, and 1251. Other code pages can be added on request.

:p.One other detail that you need to know is that FtpServer presents
the user with a virtual file system that does not have to agree
with the physical file system. Depending on the mapping that the
system manager has defined, a file name seen by an FTP client might
not be identical to the file name in the actual file system. The
user does not need to know this detail, of course.

.***********************************
.*   MULTIDOMAIN SUPPORT
.***********************************

:h1 id=multidomain.Multidomain Support

:hp2.Multidomain Support:ehp2.

:p.The FTP protocol, as originally defined, does not have any provision
for hosting multiple domains. If you wanted to host two domains, then you
had to run two servers, on two different IP addresses.

:p.To get around this problem, a draft known as RFC7151 proposes the
addition of a HOST command, to be issued by the client before providing
a username and password. The command
:xmp.       HOST myhome.com
:exmp.
tells the server that it should now act as if it was a server for the
domain myhome.com. The command
:xmp.       HOST
:exmp.
with no parameter returns us to a state as if no HOST command had been
issued. In either case, any previous user credentials are cancelled and
the user has to log in again. (Or log in for the first time.)

:p.This proposal has not been adopted as a standard, and only a few
FTP clients support it. Most command-line FTP clients do, however, have
a QUOTE command that allows the sending of a command that the client
doesn't directly support. Thus, you can still issue the HOST command
by manually entering
:xmp.       quote host myhome.com
:exmp.
to get the same effect.

:note.FTP clients that do not know about the HOST command sometimes require you
to supply a username and password before issuing any other command.
In that case you might need to enter an arbitrary or empty username/password
combination and allow the
login to fail before issuing the "quote host" and then logging in
properly.

:p.One of the motivations for introducing a HOST command is to allow
for the possibility of different authentication rules for different
virtual hosts. This is irrelevant in the case of FtpServer, which uses
a simple USER/PASS form of login, with no support for security extensions.

:p.The other motivation &emdash. the main one, in fact &emdash. is to allow a
different directory root for each virtual host. This is relevant with
a traditional Unix-style FTP server where the client sees the server
machine's file system. It is less relevant with FtpServer, which already
allows the more general case of a different virtual file system for
each user account. There is no point in having a different directory
root for each domain, because this will be overridden by the user's
directory root as soon as the user logs in.

:p.Nevertheless, there may be situations where it is desirable to have,
for example, several different "anonymous" accounts, one for each domain.
To allow for this, FtpServer has a compromise solution that makes the
HOST command optional.

:p.The compromise is to allow a username, as specified in the Setup
program and stored in FTPD.INI, to optionally have the form user@domain.
A username that does not contain a '@' is visible in all domains.
A username that does contain a '@' belongs to the specified domain.
The directory root for such a username is, of course, specified in the
Setup program.

:p.To log into the account thisuser@thisdomain, the client enters the sequence of commands
:xmp.      HOST thisdomain
      USER thisuser
      PASS xxxx
:exmp.

FtpServer treats this as follows.
:ul compact.
:li.If a user account thisuser@thisdomain exists, we use it.
:li.If no such account exists, we look for the account thisuser.
:li.If that too fails, we have an invalid login.
:eul.
:p.If, on the other hand, no HOST command has been issued, FtpServer
looks directly for the account thisuser.

:p.There is one other possibility. If the user logs in with
:xmp.      USER thisuser@thisdomain
      PASS xxxx
:exmp.
then the part after the '@' overrides any HOST command that might have been given.

.***********************************
.*   SETTING UP THE USER PERMISSIONS
.***********************************

:h1 id=setupusers.Setting up the User Permissions

:hp2.User Permissions:ehp2.

:ul compact.
:li.:link reftype=hd refid=UPconcepts.General concepts:elink.
:li.:link reftype=hd refid=PMconfiguser.Setting up users with the Setup program:elink.
:li.:link reftype=hd refid=configuser.Setting up users with the VIOSetup program:elink.
:li.:link reftype=hd refid=configmanual.Manual configuration:elink.
:li.:link reftype=hd refid=manager.Manager privileges:elink.
:li.:link reftype=hd refid=UsingTemplates.Using templates:elink.
:eul.

.***********************************
.*   GENERAL CONCEPTS
.***********************************

:h2 id=UPconcepts.User permissions: General concepts

:hp2.GENERAL CONCEPTS:ehp2.

:p.
Each user of the server has a login name (username), a password, and
a tree of accessible directories. Typically this tree consists of
one home directory and all of its subdirectories, but more complex
arrangements are possible. The system manager may, by the use
of symbolic links, allow the tree to cross multiple drives or even
multiple nodes on the local area network. The system manager may also
allow access to a directory but block access to specified subdirectories
of that directory. In all cases, a user is restricted to using the
directories that the system manager has specified for that user. Users
cannot get at, or even see, any other directories in the machine's file
system. Furthermore, users are not told the true physical paths of
those directories that they are allowed to see.

:note.
In this context, "user" refers to a username rather than to
a person. For example, you might have a number of different
people all accessing the server via the username "anonymous".
As far as the server is concerned, they are not separate users,
but rather separate instances of the user called "anonymous".

:p.
The server looks up the user information in an INI file called FTPD.INI or FTPD.TNI,
which is in the program's working directory. (Normally this is the same
directory as ftpd.exe, but for some special cases you might choose to
start ftpd.exe from a different directory.) This INI file is created
and maintained by the Setup or VIOSetup utility, as explained below.

:p.
There are two ways to create and edit the user permissions.
:ol.
:li.By using the VIOSetup or Setup program that is supplied with FtpServer.
(VIOSetup and Setup do the same job, except that one of them is a
text-mode program and the other isn't.)
This is the recommended method, for compatibility with future releases
of FtpServer, and also because this method ensures that you produce
syntactically correct permission data. The procedure is described
in the sections :link reftype=hd refid=PMconfiguser.Using the Setup utility:elink.
and :link reftype=hd refid=configuser.Using the VIOSetup utility:elink..
:li.Manually, using any text editor. The details can be found in the section
:link reftype=hd refid=configmanual.Manual configuration:elink.. Manual
configuration is supported for the benefit of existing FtpServer users who
have become used to doing it this way. Manual configuration is also
useful for installations where new user accounts have to be created
dynamically - from Rexx or Perl scripts, for example - without running the Setup
utility. For the majority of FtpServer installations, however, manual
configuration is not recommended, because it is too easy to make mistakes.
:eol.

:p.
:hp2.User categories:ehp2.

:p.
Each user is classified as one of the following.

:dl break=none.
:dt.   G
:dd.Guest user, who has to provide an e-mail address as a password.
:dt.   U
:dd.Normal user, who has to supply a password
:dt.   N
:dd.User who does not need a password.
:dt.   M
:dd.Manager: same as U, except that a manager gets some
:link reftype=hd refid=manager.extra privileges:elink..
:dt.   T
:dd.Template. Unlike the categories above, this is not a
client account that users can log in to. Instead, it is a
way of defining a set of directory permissions that can be shared by
a group of users. See also the section
:link reftype=hd refid=UsingTemplates.Using templates:elink..
:edl.

:p.There are also four special categories GS, US, NS, MS, which
are the same as above except that they denote 'single-use' accounts.
A single-use account is one that can be used only once; after the
user logs in, the server automatically deletes the account, so
that it cannot be used a second time.

:p.
Normally you would create one "manager" account for yourself, and use
the G or U categories for all other users. The N category is for those
rare cases where you don't need to control access with a password.

:p.The difference between the N and G categories is that a Guest
user is prompted for a password, which should be an e-mail address
(and FtpServer checks that it contains an '@'); while a
"No Password" user is not even prompted for a password.

:p.
:hp2.Directory permissions:ehp2.

:p.
Each directory that is accessible to the user is described by some
combination of the following five permission attributes.

:dl break=none.
:dt.   V
:dd.Directory visible. This should be set in most cases. When it is not
set, the client can't do a "change directory" to this directory, and it
will not appear in directory listings.
:nt text='Special case'. It is always legal to ask for a listing of the root
directory, but if it is invisible then the result will be an empty listing.
It is also legal to change directory to the root directory if it is
invisible, even though it is not possible to change to any other
invisible directory.
:ent.
:dt.   R
:dd.Read permission. If this is set then the client can download files
from this directory.
:dt.   W
:dd.Write permission. If this is set then the client can upload files
to this directory.
:dt.   D
:dd.Delete permission. If this is set then the client can delete files
from this directory. Note: to overwrite an existing file, both W and D
permissions are needed.
:dt.   N
:dd.Rename permission. If this is set then the client can rename files
in this directory. Note: if the rename results in moving a file to a
different directory, then the N permission flag is no longer relevant. In that
case, the user needs a D permission for the source directory and a
W permission for the destination directory.
:edl.

:nt text='Remark'. It is possible for a user to be given read, write,
delete, and/or rename privileges to an invisible directory. In such cases the users can
perform the permitted operations only if they know the correct file name,
including the directory name,
because they won't see the directory name in a directory listing.
:ent.
:p.
It is also possible for an invisible directory to have visible subdirectories.
Users can get to those directories only if they know the path name, including
the name of the invisible directory.

:p.Another way of hiding files and directories, independently of the
V attribute, is to use a HideList. This is explained in the section on
:link reftype=hd refid=PMedituser.Editing a user's permissions:elink.

:p.
:hp2.Symbolic links:ehp2.
:p.
A symbolic link is a pointer to some other part of the machine's file system.
You - the system manager - can insert symbolic links in any directory.
From the user's point of view, a symbolic link looks like just another
file or subdirectory.
:p.
A symbolic link has a name (which is what the user sees) and a physical
path (which is known only to the system manager). The physical path must
be either a null string, or a full path name including the drive letter.
:p.
A symbolic link normally points to a directory, but it is also possible
to create a symbolic link to a file that is not a directory.
:p.
Note that the user cannot see any difference between a symbolic link and
an ordinary file or subdirectory name. All that the user sees is a single
directory tree that starts with a root node called "/".

:p.
:hp2.Pseudo-directories:ehp2.
:p.
It is possible to specify a symbolic link whose physical path is unspecified.
(That is, it is an empty string.) This creates a pseudo-directory:
something that ftp clients will see as a directory, but which does not
correspond to any physical directory. A pseudo-directory cannot hold any
real files, but it can contain symbolic links.
:p.
The main use for a pseudo-directory is for the case where you want to give
a user access to several unrelated directories, possibly on different
drives. To do this, you can make the user's top-level directory a
pseudo-directory, and then put links in that directory to the directories
that the user is allowed to see. More complicated examples, using
pseudo-directories deeper down in the directory tree, are of course possible.

.***********************************
.*   MANUAL CONFIGURATION
.***********************************

:h2 id=configmanual.Manual configuration

:hp2.Manual configuration of users:ehp2.

:p.
This section describes how to edit a permission file. It can be skipped
by most people, because in most cases it is better to use the
:link reftype=hd refid=PMconfiguser.Setup:elink. or
:link reftype=hd refid=configuser.VIOSetup:elink. program to automate the editing.

:p.If the user information is already in the server's INI file (e.g.
because you used the Setup utility to add this user), then the first
thing you need to do is to use
the :link reftype=hd refid=loadstore.StorePRM utility:elink. to create
a PRM file for this user. (Of course this step is not needed if you have
an existing PRM file for the user.) After editing the PRM file, which
you can do with any text editor, you can use the
:link reftype=hd refid=loadstore.LoadPRM utility:elink. to load the
information back into the INI file.

:p.
A PRM file is free-format, i.e. the exact formatting is not
important; but, for the sake of readability, I suggest that
you use indentation etc. to make its structure clearer.

:p.
The file can include comments. A comment is anything from the
'%' sign to the end of the current line. Note, however, that comments
will be stripped out when the LoadPRM program loads the data into the
server's INI file.

:p.
File names containing spaces or special characters should be
delimited by either double quote marks ("...") or single
quote marks ('...'). For "normal" file names the quotation
marks are optional. (But see the warning later in this page.)

:p.
The first five things in a permission file are:

:ol.
:li.The user category code (G, U, N, M, T, GS, US, NS, MS), as described in the
:link reftype=hd refid=UPconcepts.General concepts:elink. section.
:li.The password. For a guest user, put "@" as the password.
For an 'N' user or a template, just supply a dummy entry here.
:li.The user limit (a numeric value).
:li.This user's speed limit (a numeric value).
:li.The user's real name.
:eol.
:p.Next, you may have some notes - up to 2048 characters - starting
with the character pair '(*' and finishing with '*)'. If the
strings (* and *) occur inside the notes, they must occur as
properly nested matched pairs.

:p.Finally you specify the directory information. If you want to use
a template, then the directory information has the form
:p.
       @<template name>
:p.
where <template name> has the form of a username, but it specifies
a "user" of type T. If you are not using a template, then the
directory information should be in the format
:p.
       <directory name> <directory descriptor>
:p.
where <directory name> specifies the user's root directory.  There
are two possible ways to specify a <directory&rbl.name>:
:ul compact.
:li.<namestring>
:li.<namestring> = <namestring>
:eul.
:p.where a <namestring> is any string of characters, optionally enclosed
in quotation marks. The first alternative - the one without the '='
sign - would not normally be used in specifying the root directory,
but it is the normal form for specifying a subdirectory (see below).
The second alternative specifies a symbolic link. In that case the
<namestring> before the '=' sign is the directory name as seen by the
ftp client, and the <namestring> after the '=' sign is a full path name,
starting with a drive letter.
:p.
At the root level, the directory name is not seen by the user in any
case, and the full path name is very often a null string, in order to
specify a pseudo-directory. Thus, a very common form of specification
for the root-level directory is simply
:p.
       ""=""
:p.
A <directory descriptor> gives the permissions for this home directory
and all of its subdirectories. It has the form

:p.
       <code> <subdirectory info>

:p.
Both of these are optional. The <code> can be any combination of

:dl compact break=none.
:dt.   V+
:dd.Directory visible
:dt.   V-
:dd.Directory invisible
:dt.   R+
:dd.Allow reads (i.e. downloads) of files in this directory
:dt.   R-
:dd.Deny read
:dt.   W+
:dd.Allow write
:dt.   W-
:dd.Deny write
:dt.   D+
:dd.Allow delete
:dt.   D-
:dd.Deny delete
:dt.   N+
:dd.Allow rename
:dt.   N-
:dd.Deny rename
:edl.

:p.
The permission codes are always to be interpreted relative to
the parent directory's permission code. That is, a directory has
the same permissions as its parent, unless explicitly changed
by adding and&slash.or deleting permissions.

:p.
(For the root directory, the default permissions are: visible,
read, no write, no delete, no rename.)

:p.
The <subdirectory info> is defined recursively. It has the form

:p.
       (  <item> ,  <item> ,  ...  , <item> )
:p.
i.e. it is a comma-separated list of items, surrounded by
parentheses. Each <item> has the form
:p.
       <directory name>  <directory descriptor>
:p.
That is, it follows exactly the same rules as described above
for the root directory. The recursive nature of the rules means,
of course, that the <directory&rbl.descriptor> for any
subdirectory may contain specifications of further subdirectories,
down to any desired level.
:p.
If this sounds complicated, take a look at the supplied *.PRM
files, and you'll soon pick up the pattern.

:note.You don't have to list all of the subdirectories - only the
ones whose permissions are different from the permissions of the
parent directory.

:p.
:hp2.Example 1:ehp2.
:p.Suppose you want the user "anonymous" to have read access to
the directory C&colon.\users\pub; read and write access to
C&colon.\users\pub\upload; no access at all to C&colon.\users\pub\private;
and read access to all other subdirectories
of C&colon.\users\pub. Then the permission file ANONYMOUS.PRM should
have the following contents.
:xmp.
    G                         % user category = guest
    @                         % password = e-mail address
    10                        % user limit
    2000                      % speed limit
    ""                        % no real name recorded
    (*This is a guest account*)   % notes to store in INI file
    pub="C&colon./users/pub/" V+R+  % user's root directory
      ( upload W+,            % allow write access to upload directory
        private V-R- )        % deny all access to private directory
:exmp.

:p.
:hp2.Example 2:ehp2.
:p.Suppose you want the user "user1" to have read and write access to
drive A&colon.; read-only access to directory C&colon.\users\pub and all of
its subdirectories; read and write access to D&colon.\abc and all of
its subdirectories; and read and delete access to E&colon.\Apps.
To make the example more interesting, let us suppose that we want to
make the directory on E&colon. look like a subdirectory of
D&colon.\abc\def. You can do this by creating a permission file
USER1.PRM with the following contents.
:xmp.
    U                         % normal user
    secret                    % password
    2                         % user limit
    6000                      % speed limit
    "Bart MacHomer"           % real name
    (*Created April 1999*)    % notes to store in INI file
    ""="" W+                  % root directory is a pseudo-directory
       ("A"="A:",
        "pub"="C&colon./users/pub" W-,
        "dir1"="D&colon./abc"
          (def
            ("apps"="E&colon./Apps" W-D+)
          )
       )
:exmp.

:p.
Remark: FtpServer considers the forward slash (/) and backslash (\) to
be equivalent in filename strings.

:p.:hp2.Example 3:ehp2.
:p.Suppose you want to give the same directory permissions to a large
group of users, where each user has a different username and password.
For this example, let us assume that these users should
be able to download files from the directories G&colon.\files\fixes
and G&colon.\files\drivers, but not from any other subdirectory
of G&colon.\files, and not from any other directory.

:p.The easiest way to do this is to create a template. Accordingly,
let us create a permission file CUSTOMERTEMPLATE.PRM, with the contents

:xmp.
    T                         % template
    ''                        % dummy password
    1 1                       % dummy user limit and speed limit
    ''                        % real name is not used
    (*Template for customer accounts*)    % notes to store in INI file
    ''=''                     % root directory is a pseudo-directory
       ('fixes'='G&colon.files\fixes',
        'drivers'='G&colon.files\drivers')
:exmp.

:p.This gives us the template, but it does not yet give us any
user accounts. To create an account that uses this template, we can
create another permission file CUST005342.PRM with the contents

:xmp.
    U                         % normal user
    'qwertyuiop'              % password
    1                         % user limit
    5000                      % speed limit
    'Asterix the Gaul'        % real name
    (*Customer number 5342*)  % notes
    @customertemplate         % directory permissions are in template
:exmp.

:p.Naturally, you can create many other user accounts that use
the same template. If you later change the template, all of the
user accounts that use that template will automatically be
changed.

:p.:hp2.Warning about potential syntax errors:ehp2.
:p.
The software that parses a permission file tries to be
as non-rigid as possible; for example, it does not insist
that the characters in passwords, directory names, etc.
be alphanumeric characters. This flexibility comes at a
price: you can write permission files that seem to be
correct, but which are syntactically ambiguous.
:p.
To avoid problems, it is a good idea always to enclose
directory and file names in quotation marks.
:p.
:hp2.Converting from older formats:ehp2.
:p.Very rarely, a new version of FtpServer will introduce a new option
for the PRM files. This creates the risk that your existing PRM files
could become obsolete. If this happens, you should convert all your old PRM files
to the new format.
:p.
The conversion can be done with the LoadPRM utility, which (in the case
of a format change) will be designed to be able to
read PRM files in either the old or new format but will store the data (in FTPD.INI)
in the new format. A quick way to convert all your PRM files into the
new format is to execute the two commands
:xmp.
        loadprm *
        storeprm *
:exmp.
:p.Note that this will strip all comments out of the files.  If you want to
keep the comments, you will have to do some manual editing.

.***********************************
.*   MANAGER PRIVILEGES
.***********************************

:h2 id=manager.Manager privileges
:hp2.Manager privileges:ehp2.
:p.
A manager account is the same as a normal user account, except that
a manager has a few extra privileges.
:ul.
:li.Managers can see system and hidden files in directory listings;
other users cannot.
:li.Managers are allowed to use the :link reftype=hd refid=SITE.SITE MNGR:elink. commands.
:eul.

.***********************************
.*   USING TEMPLATES
.***********************************

:h2 id=UsingTemplates.Using Templates
:hp2.Using Templates:ehp2.
:p.
A template is a way to allow groups of users to share the same
directory permissions. In the Setup program, you can create a
pseudo-user of type "Template". This is not a true user account,
because nobody may use the name of this template as a login name.
You can think of it as either a placeholder or as a way to define
groups. You create the directory permissions for a template in
exactly the same way as you would create them for a normal user
account.

:p.The non-directory attributes of a template account (password,
user limit, speed limit, etc.) are ignored by FtpServer, so the
values that you give those attributes are of no importance.

:p.To use a template, you can go to any other account in the
Setup program and check the "Use Template" box. You must then
enter the name of the template. Once you do this, you will find
that you can no longer edit the directory permissions for that
user. Instead, the directory permissions are a direct copy of
those in the template. If you change the permissions in the
template, they will automatically change for all user accounts
that are using that template.

.***********************************
.*   THE SETUP UTILITY
.***********************************

:h1 id=PMconfiguser.The Setup utility
:hp2.The Setup utility:ehp2.

:p.When you run Setup, you get a notebook that controls all of the
configuration details of FtpServer. The parameter settings are stored
in a file FTPD.INI or FTPD.TNI. (Which of these will be used is explained
in the section on
:link reftype=hd refid=tnifiles.TNI files:elink..)
When you change these parameters, existing user
sessions will continue, in most cases, to use the old settings, but
future clients will be affected by the changes. This means that you
can do things like adding users or changing user permissions while
the server is running.

:p.There are certain parameters that cannot be changed while the
server is running. These are:
:ul.
:li.the server port number
:li.the user log options
:li.the option to bind the server to a specific IP address
:li.the free space threshold, which determines whether there is
enough free disk space to permit an upload to be performed
:eul.

:p.You may still change these parameters in the Setup program, but
if the server is running at the time then
the changes will not have any effect until after you have shut down
and restarted the server.

:p.The opening screen of Setup gives you a choice between local and
remote configuration. (The remote case is described in a
:link reftype=hd refid=remoteconfig.later section:elink..)
If you want to bypass this choice, you can use the command
:xmp.         setup -G
:exmp.
and this will skip the opening dialogue and continue as if you had
pressed the "GO" button. In this case, the local/remote option remains
as it was the last time you ran Setup. Alternatively, the command
:xmp.         setup -L
:exmp.
will force local configuration, and the command
:xmp.         setup -R
:exmp.
will force remote configuration. In the present version of Setup these
command-line options can be either upper or lower case, and the '-'
character is optional, but this could change in future versions.

:p.Two further command-line options, which may optionally be combined
with one of the above, are the "T" and "I" options:
:xmp.         setup -T
         setup -I
:exmp.
This specifies that Setup should operate on FTPD.TNI or FTPD.INI,
respectively. More importantly, this decision is remembered, so that
in future, when running ftpd.exe or setup.exe or viosetup.exe, you don't need
to say whether you want the INI or the TNI format.

:p.The setup details are divided into several groups.

:ul compact.
:li.:link reftype=hd refid=PMconfiguser1.Basic:elink.
:li.:link reftype=hd refid=PMlogging.Logging:elink.
:li.:link reftype=hd refid=PMconfiguser1a.Options:elink.
:li.:link reftype=hd refid=LocalAddrs.Local addresses:elink.
:li.:link reftype=hd refid=PMsecurity.Security:elink.
:li.:link reftype=hd refid=PMconfiguser2.Users:elink.
:eul.

.***********************************
.*   THE BASIC SETUP SETTINGS
.***********************************

:h2 id=PMconfiguser1.The basic server settings
:hp2.The basic server settings:ehp2.
:p.
The "Basic" page controls the following parameters.

:dl break=all.
:dt.Port number
:dd.This is the tcp port on which FtpServer listens for new connections.
Unless you are doing something nonstandard (for example, running two ftp servers
on the same machine) this should always be 21.
:dt.Maximum number of users
:dd.This specifies how many clients will be allowed to use the server simultaneously.
I usually set this to 10, but you would set the limit much higher if you are
running a dedicated server for many clients. Higher values will, of course, increase the potential
load on your processor.
:p.Note: this is a global maximum. You may also set this to a high value, and then
control the number of users on a per-username basis.
:dt.Maximum number of guest users
:dd.This typically should be less than the number specified for the maximum
number of users, to reserve one or more login slots for the system manager and other
non-guest users.
:dt.Free space threshold (MB)
:dd.This specifies the amount of free space that must be available on a drive for
uploads to be enabled. If the free space, in megabytes, falls below this level then
uploads will be disabled.
:dt.Timeout (seconds)
:dd.The time that a client session may remain idle before the user is evicted.
You will find that many ftp clients, especially web browsers, don't log out properly,
so their sessions have to be killed with the timeout mechanism. You may specify an
interval of up to 4294967 seconds (about 49 days), but normally you would want to
eject idle clients much earlier than that. In most situations a value of 300
seconds (5 minutes) is a suitable compromise. Do not set the value too much lower
than that, because you have to allow for human response time.
:p.Note that the time spent uploading or downloading a file is not counted as
idle time. Even if that transfer takes many hours, there will not be a timeout
provided that each "chunk" of that file is transferred reasonably often.
:edl.

:p.Next, we have a couple of checkboxes.
:dl break=all.
:dt.Use multiple INI files
:dd.Normally the data for all user accounts are kept in the main INI file called
FTPD.INI or FTPD.TNI, and most people will want to leave it that way. With this option activated, the account
data are spread over many different INI files, which can improve
access speed in the case when you have hundreds or
thousands of ftp user accounts.

:note.Here and elsewhere in this manual, any reference to "INI files" refers
to files that can be either in INI or TNI format.

:p.Note that switching this option on or off forces a major rearrangement
of the INI data. This can take a long time, especially when working on a
large INI file. You should wait for the conversion to finish before
exiting from the Setup notebook.
:dt.Disable Telnet compatibility
:dd.This option is for use with servers using the Windows-1251 character
set to support file names in Cyrillic. Everyone else should probably
leave it turned off. The FTP standard requires servers to be compatible
with the standard for Telnet; that is, there might be Telnet commands
embedded in the command stream. (In particular, FTP clients are allowed
to generate the Telnet "Interrupt Process" command.) The first byte of
a Telnet command is a byte with code 255 (0FF hexadecimal). This normally
causes no conflict, because that code is unused - because it is reserved for Telnet - in all standard character
sets. Unfortunately, it is the code for the Cyrillic letter 'ya' in
the nonstandard code Windows-1251. That means that Russians, and anyone
else using Cyrillic letters, must choose between doing without Telnet
and doing without the letter 'ya'. Most of them have chosen to do without
Telnet. If this applies to you, you must activate this option in order
to have access to files whose names include the letter 'ya'.

:p.In principle, a better solution is to abandon Windows-1251 and move
to a standard character code such as ISO-8859-5. In practice, Microsoft
has won its battle against standards in Eastern Europe, and the ISO codes
are not popular with users.
:edl.

.***********************************
.*   LOGGING
.***********************************

:h2 id=PMlogging.Logging
:hp2.Logging:ehp2.

:p.FtpServer can produce three different log files. You can enable any
combination of these three.
:ul.
:li.The user log file contains a summary of the upload and download
information for each user session.
:li.The "common log" contains roughly the same information as the user
log, but in a format that is commonly used by web servers. You can use
this format if you use web server analysis tools to produce a
periodical summary of the transfers.
:li.The transaction log is a more detailed log of every operation
performed by the clients. If you write the transaction log to disk,
you can use it for troubleshooting operations such as tracing
attempts to violate your system security. You should be aware,
however, that detailed logging can produce very large log files.
If you use this option, you should archive or delete the log files
at frequent intervals. If you write the log information to the
screen, you can get an idea of how busy your ftp server is.
:eul.

:p.You may safely delete or move these log files. FtpServer
re-creates the files if they are missing. It would be a good idea
to move these files to an archive directory once a month, or more
frequently if you have a busy server. There are many available
software products (see, for example, DragText) that can schedule
jobs to be done once per month, or once per week, etc.

:p.:hp2.Transfer logging:ehp2.
:p.You can ask the server to
produce a user log (FTPUSERS.LOG) and/or a log in common log format (COMMON.LOG).
(If you delete these files, they will be re-created. It would be a good idea to
delete them periodically, or move them to an archive, so that they do not grow
too large.) The user log produces a list of files that have been uploaded
or downloaded. The common log contains similar information, but in a format
used by many http servers. This allows you to use log analysis tools that
have been designed for web servers.

:p.You can select how much detail gets written to these two logs.

:dl break=all.
:dt.    No transfer logging
:dd.This effectively disables the transfer logging.
:dt.    Log successful transfers
:dd.The log includes entries for all uploads and downloads that completed
successfully, but the operations that failed are not logged.
:dt.    Log all file transfers
:dd.With this option, you get log entries even for transfers that were
aborted before they completed.
:dt.    Log all clients
:dd.This creates user log entries for all users, even those who didn't transfer any files.
:edl.

:p.The names FTPUSERS.LOG and COMMON.LOG mentioned above are the default
names of the log files. As you will see when running the Setup program,
you have the option of specifying different names. Similarly, you are
free to change the name of the disk file that holds the transaction log (see below).
All three of the file names that can be specified on this page may include
directory specifications, either relative to the current working directory
or as absolute path specifications. This permits you to keep the
log files in different directories if that is what you prefer.

:p.:hp2.Transaction logging:ehp2.
:p.The transaction log is a much more detailed log. You can choose to send it
to the screen, to a disk file, to a pipe, to the syslog daemon, or any combination of these.
The disk file, which is called FTPTRANS.LOG unless you change its name in the
setup notebook, is updated approximately once every
minute if this feature is enabled. While the server is running, you
might notice that a file with a name like FTPTRANS.$$$ is created. This
is a temporary file containing the most recent transaction log entries.
We distinguish between the short-term log and the final log for two
reasons: to avoid overheads associated with writing to very large
files, and to avoid sharing violations that might otherwise occur if
you tried to read the transaction log while FtpServer has it open.

:p.Warning: Transaction logging can create very large log files. I suggest that
you don't enable transaction logging to a file unless you are trying to track
down a problem, or unless you regularly delete the transaction log.
Logging to the screen, on the other hand, will give you some idea of how busy
the server is. Logging to the screen does add some processor overhead,
but you are unlikely to notice this unless you have a very busy server.

:p.If you choose the "pipe" option, the log is sent to a named pipe
called \PIPE\FtpServerTranslog. For this option to be useful, there
needs to be another program running that reads the data from the pipe.
Thus, this is a suitable option if you plan to write extra software
that analyses or reorganises the transaction log data. For some example
code on how to use a pipe, look for "pipelog.zip" in the "Weasel Tools"
section of the site where you got this program.

:p.If the syslog option is enabled, you would normally want to leave
the syslog host name set at 127.0.0.1 (the local host). You may, however,
log to a different machine by specifying a hostname or a numeric IP
address in the "Syslog host" field.

:p.For the "syslog" option to be useful, the syslog daemon should be
running on the machine specified as the syslog host. This can either
be the IBM-supplied syslogd, or any replacement
that reads its data from UDP port 514.

:p.The checkbox at the bottom of the screen controls whether the
:link reftype=hd refid=SITE.SITE MNGR:elink.
commands are included in the transaction log. This gives you the
option of suppressing
the many SITE MNGR commands that occur when you are running the
:link reftype=hd refid=Monitor.Monitor:elink. utility.

.***********************************
.*   MORE SETUP OPTIONS (PM)
.***********************************

:h2 id=PMconfiguser1a.Options
:hp2.Options:ehp2.
:p.:hp2.IP Address Binding:ehp2.

:p.The first option on this page is a choice between
binding to all local interfaces, or binding to a specific IP address. If you
choose the "specific address", you should enter an IP address in the
standard format (four decimal numbers, separated by dots).

:p.For most applications, the best choice is "all interfaces". With this
choice the server listens for ftp requests on all your network interfaces,
even if your machine has multiple IP addresses. The most common reason
for having multiple addresses is that your machine has both a LAN
address - i.e. it is part of your local network - and a global IP
address that makes it visible to the global internet. If this is your
situation, the "all interfaces" option makes your server visible both
on the Local Area Network and on the global internet.

:p.The "specific address" option is for the case where you have two or more
IP addresses, but you want the server to respond to only one of them. In
this case you could, if you wished, run several independent ftp servers on
the same machine, each responding to a different address.

:p.(Another way to run several ftp servers is to make each one listen on
a different port. That is a less attractive option, however, because most
ftp clients expect to find the server on the standard port 21.)

:p.If you do run multiple copies of ftpd.exe, run them from
different directories. (This does not require having separate physical
copies of ftpd.exe.) This is because ftpd.exe expects to find its INI file
in the current working directory, and for multiple copies you
would want to have a different INI file for each one.

:p.:hp2.Restricting the PASV port numbers:ehp2.

:p.To understand this option, you should read the
:link reftype=hd refid=UnderstandingPorts.Understanding Port Numbers:elink.
section of this manual. You should probably also read the documentation
that came with your firewall and/or router software or hardware, because this option is most
commonly used in connection with a firewall or router.

:p.If you are running a firewall, or sometimes even if you do not have
a firewall, you might want to control which ports are used for a
passive data transfer. If you disable this option, you are trusting the
tcp/ip stack to choose the 'next available port' for a passive data
transfer. If you enable it, you restrict the possible port numbers that
can be chosen.

:p.Unless you have expert knowledge for setting up non-typical scenarios,
you should stick to the following guidelines:
:ol.
:li.For the majority of applications, the acceptable set of port numbers
lies in the range 49152 to 65535. (If you are running a firewall, and
you want greater security, you will probably want to use a smaller range. In that
case the range must agree with what the firewall will let through.)
Port numbers in the range 0 to 1023 should probably be never used, because
those ports are reserved for 'well-known services' such as SMTP and Telnet.
Port numbers from 1024 to 49151, inclusive, are used for servers that
are less 'official', but which nevertheless have well-defined ports. The
ports from  49142 onwards are available for non-server applications,
therefore these are the ones you should use for your data transfers.

:li.If you enable this option, the data port used by the server for
passive ftp is restricted to the specified range. This range should
match a range of ports that the firewall will let
through, and normally it should be a range that is not in use for some
other purpose. In addition, you must tell your firewall to forward
connections on these ports :hp3.without any port alteration:ehp3..
Note that this has no effect on non-passive ftp,
where port 20 is always used as the server's data port.

:eol.

:p.:hp2.Working behind a firewall:ehp2.

:p.The 'behind firewall' mode of FtpServer is designed specifically to
deal with the case where you have a firewall or router that implements Network
Address Translation (NAT). Before enabling this option, make sure
that you have read the
:link reftype=hd refid=UnderstandingPorts.Understanding Port Numbers:elink.
section of this manual. You should probably also read the documentation
that came with your firewall or router software.

:p.Remark: in this section the term "firewall" is used, for brevity, to
mean any implementation of NAT. This could be a firewall, or a router, or
some combination of these. That is, the "behind firewall" option could be
needed even if you don't have a firewall.

:p.Note, especially, that some firewalls are smart enough to intercept
FTP commands and responses and to adjust them to compensate for the
address translation. If you have a firewall that implements NAT
completely, you should leave this FtpServer option turned :hp3.off :ehp3.. The
option is needed only if you have to compensate for the limitations of
your firewall. You should therefore enable this option only if you
discover that the server will not work with it turned off.

:p.If you need this option, you will probably also need to activate the
option to restrict the port numbers, as explained earlier on this
page.

:p.If the 'behind firewall' option is enabled, you will get the following
change:

:ul.

:li.A new
:link reftype=hd refid=LocalAddrs.Local addresses:elink.
page is added to the Setup notebook, to specify the details of the
"behind firewall" operation.


:li.In response to a PASV command, the server does not report its
own IP address. Instead, it reports the IP address of the
firewall. Since the server has no way of discovering the IP
address of the firewall, this must be entered manually in this
Setup program.

:eul.

:p.Note that this option affects only passive ftp. When we have
our server behind a firewall or router, we don't have to do anything special
to make non-passive ftp work correctly.

:p.:hp2.The anti-tagger option:ehp2.

:p.This option was added in response to the actions of software
pirates, who appear to have ways of searching the world for FTP
servers that have write-enabled 'incoming' directories. They
upload pirated files, and then (presumably) tell other people
where to download those files from.

:p.If you enable this option, FtpServer checks for uploads that
are typical of pirate operations. It considers that a pirate has
been detected on any of the following conditions.
:ul.
:li.the creation of a directory whose name includes
the string 'tag'. This includes the name 'tag', but also
things like 'taGGed by kewl d00dz'
:li.the creation of a directory whose name is 12 digits followed by
the letter 'p'
:li.the creation of a directory without uploading any files into
that directory
:li.the uploading of a file whose name (modulo alphabetic case) is
SPACE.ASP or is a name including the substring TEST.PTF
:li.the uploading of a file whose name contains only numeric digits
or the '.' character
:eul.

:p.If any of these conditions is satisfied, FtpServer takes two actions&colon.
:ul.
:li.It immediately reduces the speed limit for this session to
a low value - not so low that the user will think the server has
hung, but low enough to be an inconvenience to the user.
:li.It allows the uploads to continue, but at the end of the session it
deletes all files and directories added during that session.
:eul.

:p.If you use this option, you should of course tell your
legitimate uploaders not to create subdirectories or upload files
whose names match the names used by the pirates.

:p.:hp2.The 'Hide passwords' option:ehp2.

:p.If you check the 'Hide passwords' box, user passwords will not be
visible in the 'edit user' dialogue. In addition, password encryption will be
turned on by default when you add a new user (although you can choose
to turn it off again before entering the password).

:p.This is a reversible change. If you need to check someone's password,
you can turn this option off and then go to the 'Users' page. Note,
however, that there is no way to re-read a password that has already been encrypted.
If you unhide an encrypted password, all you will see is the encrypted form.
Hiding and encryption are two separate options.

.***********************************
.*   LOCAL ADDRESSES
.***********************************

:h2 id=LocalAddrs.Local Addresses
:hp2.Local Addresses:ehp2.

:p.This page appears in the Setup notebook only if you have enabled
the "behind firewall" option.

:p.In this situation, the server's external address - that is, the address
seen by the client - is different from its internal address in the LAN.
The external address is known to the firewall or router, but FtpServer has
no way of finding it out. Therefore, you need to fill in the external address
in the first field on this page.

:p.The listbox below this specifies the client addresses that should
:hp3.not:ehp3. be subject to the "behind firewall" rules, usually because
they connect to the server via the LAN, without going through the firewall
or router. Most typically that listbox will require just two entries,
saying that you should allow a range of local addresses, and exclude all
others from the "local addresses" set. More complicated rules might be
needed if you have a complicated LAN.

:p.This list is edited in exactly the same way as the similar list on the
:link reftype=hd refid=PMsecurity.Security:elink. page.

.***********************************
.*   SECURITY SETTINGS (PM)
.***********************************

:h2 id=PMsecurity.Security settings

:hp2.Security settings:ehp2.

:p.The settings on this page allow you to put restrictions on the
addresses from which a client may connect to the server. You will want
to enforce such restrictions if, for example, you want the server
to be available only from inside your own LAN. Another common reason
for enforcing such restrictions is to lock out nuisance users who
are trying to use the server for unethical purposes.

:p.When a client is rejected as the result of the checks on this page,
there is a 30-second delay before the rejection message is sent back.
The reason for this delay is to control denial-of-service attacks,
where a hostile client keeps re-attempting to connect.

:p.:hp2.Maximum simultaneous connections from a single client address:ehp2.

:p.
The first item on the "Security" page is a field called
"Max connections from same address".  This specifies the
maximum number of users that can be connected simultaneously from the same IP
address. It is primarily a protection against users who hog the server by
logging in more than once. I usually set it to 2 or 3. If you do not
want this protection, set the number to a very large value.

:p.:hp2.Restricting access to certain IP addresses:ehp2.
:p.
The large box on this page defines a filter for IP addresses. This is for
putting restrictions on which remote hosts are allowed to log into the server.
(If you don't need this feature, just use a single "Allow all" entry.) When a
client tries to connect, the server searches this list, starting at the
beginning, for the first entry that matches the client's IP address. There will
always be a match, because the last entry is always an "everything else" entry.
The allow/refuse flag on the matching entry is used to decide whether the
client should be allowed to connect. If the flag value is "refuse", the
connection attempt is rejected.

:p.The Edit, Insert, Promote, and Delete buttons have the obvious meanings.
The details of how an individual entry is specified are given on the
:link reftype=hd refid=IPFilter.following page:elink..

:p.Because the list is always searched from the beginning, and because
the search stops as soon as a match is found, the order of
entries in the list is important. In most cases you should put the
most specific rules at or near the beginning of the list, and the more
general rules later in the list.

:p.:hp2.Example 1.:ehp2. If you want to lock out all machines with IP addresses in
the range 123.45.67.0 to 123.45.67.127, your list would look like this.
:sl compact.
:li.Refuse   123.45.67.0   255.255.255.128
:li.Allow    all others
:esl.
:p.Equivalently, you could write this as
:sl compact.
:li.Refuse   123.45.67.0 / 25
:li.Allow    all others
:esl.

:p.:hp2.Example 2.:ehp2. Suppose you want to give access only to your local network,
which has addresses in the range 123.45.66.0 to 123.45.67.255. You can do this as
follows.
:sl compact.
:li.Allow    123.45.66.0 / 23
:li.Refuse   all others
:esl.

:p.:hp2.Example 3.:ehp2. To allow access to 123.45.67.89, but to lock out everyone
else in 123.45.67.*, you can use the rules
:sl compact.
:li.Allow    123.45.67.89
:li.Refuse   123.45.67.0    255.255.255.0
:li.Allow    all others
:esl.

:p.Notice that the list always finishes with an "all others" entry. The Setup
program will allow you to change the allow/refuse flag on this final entry, but
it will not allow you to delete it.

.***********************************
.*   IP FILTER ELEMENTS
.***********************************

:h3 id=IPFilter.Form of an address filter list element

:hp2.Form of an address filter list element:ehp2.

:p.Each entry in an IP address filter has an allow/refuse flag and a
specification of which addresses are covered by this entry. For the
purposes of this Setup dialogue, an IP address is always
expressed in "dotted quad" notation: a four-byte value
where each byte has its value
written out in decimal. (This is a standard convention for writing IP addresses.)
The most significant byte comes first.

There are several different ways of specifying a list entry.

:p.:hp2.Single:ehp2.

:p.This is for the case where you want to specify just one IP address.
The address must be in the dotted quad notation mentioned above.

:p.:hp2.Masked:ehp2.

:p.This is one way of specifying a group of addresses. Here, you specify a
base IP address and a binary mask, both in dotted quad notation.
A client address matches an entry if
:p.       (client IP address) :hp2.AND:ehp2. mask = (IP address in the list)
:p.where :hp2.AND:ehp2. means the bit-by-bit Boolean "logical AND" operation.
:p.Note, in particular, the two extreme cases:
:ul.
:li.If the mask is 255.255.255.255, then we are specifying an exact match between
the client IP address and the address in the list. That is, this case is
equivalent to specifying the "Single" option.
:li.If the mask were 0.0.0.0, then any IP address would match this entry. The last
entry in the list is implicitly of this form, to specify an "everything else" condition.
:eul.

:p.:hp2.CIDR (Classless Inter-Domain Routing notation):ehp2.

:p.With only rare exceptions, the "masked" notation usually uses a
mask which is a sequence of 1 bits followed by a sequence of 0 bits.
In this case it is easier to specify the number of leading 1 bits rather
than giving the mask explicitly. This what the CIDR notation does. (The
"classless" in the name refers to a relaxation of some older rules that
only allowed a few different sizes for a subnetwork.) For example, the
notation
:p.       192.168.1.0 / 24
:p.specifies a group of addresses ranging from 192.168.1.0 to 192.168.1.255.
The "24" in this example says that the group is specified by the leading
24 bits in the 32-bit address, with every possible value for the remaining 8 bits.

:p.:hp2.Range:ehp2.

:p.With this option you can specify a range of addresses. Enter the first
and last address, inclusive, in dotted quad notation.

.***********************************
.*   MODIFYING USER PERMISSIONS (PM)
.***********************************

:h2 id=PMconfiguser2.Adding and removing users
:hp2.Adding and removing users:ehp2.
:p.
The "Users" page controls who is allowed to log in to the server.
:p.
:hp2.Deleting a user:ehp2.
:p.
Select the entry you want to delete, then click on the "Delete" button.
:p.
:hp2.Adding a new user:ehp2.
:p.
Click on the "Add" button, and then proceed as for :link reftype=hd refid=PMedituser.Editing a user's permissions:elink..
:p.
:hp2.Cloning an existing user:ehp2.
:p.
First select the user whose details you want to duplicate, then click on the "Clone"
button, and then proceed as for :link reftype=hd refid=PMedituser.Editing a user's permissions:elink..
This is the same as adding a new user, except that the new user's attributes are copied from
those for an existing user.
:p.
:hp2.Editing the permissions of an existing user:ehp2.
:p.
Select the user name, click on the "Edit" button, and then follow the instructions
in the section :link reftype=hd refid=PMedituser.Editing a user's permissions:elink..
Alternatively, just double-click on the user name.
:p.
:hp2.Sorting the list:ehp2.
:p.
This is explained on the
:link reftype=hd refid=SortUsers.next page:elink.

.***********************************
.*   THE SORT USERS DIALOGUE
.***********************************

:h3 id=SortUsers.Sorting the list of users
:hp2.Sorting the list of users:ehp2.
:p.
In the "Users" notebook page, there is a button labelled "Sort". If you
press on that button, you will get a new window that again shows the
list of users, but in a different format. It does not show all the settings
for a user - for that, you need the "Edit" button on the "Users" page -
but it gives three details for each user&colon. the username, the user
category, and a timestamp for when that user last logged in. One use for
this information is to allow you to do a quick check for accounts that
should be closed because they are no longer being used.

:p.Above the list are three pushbuttons, which also serve as column headings.
Clicking on a button causes the list to be sorted using that column as a key.
A second click on the button will sort the list in descending order. If you
click on a button several times, the sort will alternate between ascending
and descending order.

:p.To ensure that the order is retained between executions of Setup,
even if you are using multiple INI files, each user is assigned a sequence
number as Setup is closing. This can slow down the close operation. The delay
is not noticeable in most cases, but it can rise to several seconds if you
have multiple INI files and many users.

.***********************************
.*   EDITING A USER'S PERMISSIONS (PM)
.***********************************

:h2 id=PMedituser.Editing a user's permissions
:hp2.Editing a user's permissions:ehp2.
:p.
You get to this point by running the Setup program and choosing any of the
options (except "Delete") on the "Users" notebook page.
:p.
The first item in the resulting dialogue is a client category - NoPassword,
Guest, User, Manager, or Template. The categories are explained in the
:link reftype=hd refid=UPconcepts.General concepts:elink. section.
Underneath, there is an indication of when this user last logged in.

:p.Next, there is a button labelled "Add IP address controls". If you
click on this button, you will get a dialogue that is similar to that on the
:link reftype=hd refid=PMsecurity.Security:elink.
page. You can then specify rules that say which IP addresses this user
is allowed to connect from. The difference between this feature and the
one on the Security page is that the rules on the Security page are
global - that is, they apply to all connections - while the present
rules supplement the global rules by allowing you to add restrictions on
a per-user basis.

:p.The next button allows you to create or modify the HideList for this
account. The HideList is explained later in this page.

:p.To the right of this there is a checkbox labelled 'Inactive account'. You
should normally not enable this option. If this box is checked, the
account for this user is retained in the INI file, but the user
will not be able to log in. This is for the case where you want
to temporarily disable an account. It is equivalent to deleting
the user account, except that the details are saved for when you
want to re-enable the account.

:p.The next checkbox is labelled 'Limit logins'. You
should normally leave this checkbox untouched. It is for the very
special case where you want to create an account that will
self-destruct after being used a specified number of times.
See below for more details.

:p.Next you have several entry fields.
:dl break=all.
:dt.    Speed limit
:dd.An approximate upper bound on the average file transfer speed for this user,
in case you want to restrict how much of the processor power this user
can get. There are separate limits for uploads and downloads.
If you don't want such a control, just make the limits large
numbers. :note.There is no way to have a precise control over the
instantaneous speed. The user might well see peak speeds that are
higher than the limit; we can control only the average, not the peak.
:dt.    File size limit
:dd.The maximum size of a file uploaded by this user. If you enter a value
of 0, this will be treated as meaning that there is no limit.
:dt.    User limit
:dd.The maximum number of simultaneous sessions with this user name.
(If you don't want such a control, just make this number larger than the
global user limit.)
:dt.    Real name
:dd.This field is purely for your own records.  It is not used by FtpServer.
:dt.    Username
:dd.The name that the user will use when logging in. This is limited to
256 characters; more typically it's much shorter than that. A username
containing an '@' character is legal, but should be used only in the
:link reftype=hd refid=multidomain.multidomain:elink. case where
you want to use the same name in more than one domain.
:dt.    Password
:dd.This user's password. Note that this entry field is disabled if the
user category is "NoPassword" or "Guest" or "Template".
:dt.    Notes
:dd.Use this for any purpose you wish.
:edl.

:p.The password might or might not be visible, depending on whether
you chose the 'Hide passwords' option on the Options page. If you did
enable that option, the password is shown as '*' characters. Whether
or not it is hidden, you also have an 'Encrypt password' checkbox
underneath the password. This will be explained later on this page.

:p.Warning: If you change the user name, the permissions for the previous
user name will be deleted. You should also avoid using a user name
that is the same as for some other user.

:p.There is also a checkbox labelled 'Suppress log'. If this option
is enabled then commands from this user will be omitted from the
transaction log (except for the log entries showing the start and
end of the session). You can use this option when you want to
analyse the log for unusual behaviour and don't want to be bothered
logging the actions of trusted users. (You will possibly also want
to suppress logging of some Monitor.exe operations, but that is a special
case that is covered by the "Log SITE MNGR commands" option on the
:link reftype=hd refid=PMlogging.Logging:elink.
page.)

:p.The small window near the bottom of this dialogue gives a summary - but not a complete
description - of the top level of this user's directory tree. To see
the complete details, and to modify those details, click on the "Edit directories"
button or double-click on the summary window.

:p.Instructions for modifying the user's tree are on the
:link reftype=hd refid=PMedittree.next page:elink. of this document.

:p.Finally, there is a checkbox labelled "Use template". If you want the
directory permissions to be controlled by a template you should enable
this box, and fill in the name of the template in the entry field to
the right of the checkbox. If you do not use a template, or if you are
creating a template, you will need to select the "Edit directories"
button to define the directory tree. If you do use a template, the
tree is already defined for you, therefore the "Edit directories" option
is disabled.

:p.Note that none of your changes will be stored until you have confirmed
them with the "OK" button. To leave this dialogue without making any
changes, use the "Cancel" button.

:p.:hp2.Encrypting the password:ehp2.

:p.If you choose the 'Encrypt password' option, just under the password
entry field, the password you enter will be stored in encrypted form.
This is an option that is selected independently for each user; that is,
you are allowed to encrypt passwords for some users but to leave them
unencrypted for other users.

:p.The encryption is a one-way encryption, and cannot be reversed. Once
you have encrypted a password, there is no way to discover what the
password was. If users lose their password, and it was encrypted, the
only solution is to give them a new password.

:p.Note that there is also a 'Hide passwords' option on the Options
page of the Setup notebook. This is a global option that applies to
all users, and you can turn it on and off. Hiding and encrypting are
two independent options, and you can choose either or both of
these options. Hiding simply controls what is visible in the Setup
notebook. Encryption is a stronger form of privacy, because the
encryption is irreversible. If you encrypt a password but do not
hide it, you will see the unencrypted password as you type it in,
but once you select the 'OK' button you will never see the original
password again.

:p.:hp2.The HideList:ehp2.

:p.If you press the button labelled "Add HideList" or "Modify HideList"
you will be able to specify a list of file or directory names that will always be
invisible to this user. The user is also prevented from reading,
writing, deleting, or renaming these files.
Of course you can also do this with the permission
flags. What makes the HideList different is that names in the HideList are
invisible and inaccessible in every directory that the user can see. That is, a single
entry in the HideList covers all files and subdirectories with that name in all directories.

:p.Entries in the HideList may not contain the characters '/' or '\'.
(If you try to specify those characters, they will be ignored.) That is,
you cannot specify paths in the list; only file names relative to
their own directory.

:p.The wildcard characters '?' and '*' are allowed. A '?' matches any
single character, and a '*' matches any string of zero or more
characters. So, for example, "*.htm*" would prevent the listing of
any HTML files, and "???*" would hide any names of three or more
characters in length.

:p.(Obviously you should not include the quotation marks if you
copy these examples.)

:p.As a special case, it is not possible to hide the name ".." using
the HideList. (You can, of course, hide it in particular directories
by using the V permission flag.) The reason for this rule is that
many people like to hide all file names starting with a '.', but
still want a ".." to be handled normally. An entry of ".*" in
the HideList will give this result.

:p.It is not possible to do a "change directory"
to a directory whose name is hidden by the HideList. You can,
however, change to a subdirectory of that hidden directory provided
that you know how to specify the path. (Which requires knowing what
the hidden name is.) This is consistent with the way the V directory
permission works. Of course in such a case you cannot move back up the
tree with a "CWD ..", because that would take you to the hidden
directory if it were permitted. You must instead do a "CWD ../..",
or the equivalent in your ftp client software, to jump around the
hidden directory.

:p.:hp2.Limited-use accounts:ehp2.
:p.These were called single-use accounts in an earlier version of
FtpServer, but you can now allow for more than one use. A limited-use
account is one that will be deleted or deactivated after a specified number of uses.
If you check the "Limit logins" checkbox, you will then be given
a choice of "Delete account after N logins" and "Make inactive after N logins", where you can
specify the value of N. Each time that this user logs in, the value
of N is decreased by one. Once N gets to zero, the account is deleted
or made inactive, as specified.
Before this happens, you may of course go back into Setup and change
the value of N.

:p.If you set N to 0 on this page, the account is not deleted or deactivated. (That
would be too drastic an action for something that might have been a
typing error on your part.) Instead, the account reverts back to a
normal account with an unlimited lifetime. Similarly, if the N field
is still blank, or starts with a non-numeric character, at the time
you hit the 'OK' button, this too is
interpreted as meaning an unlimited lifetime.

:p.If you want to automate the creation of limited-use accounts, see
the scripts OneTime.CMD and LimitedUse.CMD in the Tools directory for examples of how
to do it.

:p.:hp2.Converting from older formats:ehp2.
:p.Very rarely, a new version of FtpServer will introduce a change
in the INI file format, because for example a new option has been
introduced. When this happens, the user data have to be updated.
:p.
The Setup program will automatically perform the conversion each time
you edit a user. If you have a small number of users defined, then the
way to do the conversion is to run Setup and edit each user (without
necessarily making any changes).
:p.
If you have a large number of users, it is easier to use the LoadPRM
program to do the conversion. This can be done with the following
sequence of commands.
:xmp.
        storeprm *
        loadprm *
        del *.prm
:exmp.
:p.The final deletion can be omitted if you prefer to keep a copy of
the PRM files. If ever your INI file is damaged or destroyed, you can
use the LoadPRM utility to re-load user data from PRM files.

.***********************************
.*   EDITING A DIRECTORY TREE (PM)
.***********************************

:h2 id=PMedittree.Editing a directory tree
:hp2.Editing a directory tree:ehp2.

:p.You get to this point while
:link reftype=hd refid=PMedituser.editing a user's permissions:elink..
The picture that is shown on the screen is a representation of the
directory tree for this user. Initially it will show enough subtrees
to reveal all symbolic links, and all entries for which the user's
permissions are different from the parent node's permissions. (For
a new user, there will be nothing except an empty root node.) In
the course of editing this tree you can expand or collapse nodes to
control how much detail is shown on the screen.
:p.
The top of the screen shows both a physical path and a virtual path
for the current entry. The physical path is the true location of
the file or directory on your disk. The virtual path is the path
as the client sees it.
:p.
Each entry describes one directory or file. At the left of each line,
you will see a code consisting of one or more of the letters "VRWDN".
The meanings of these user permission codes are explained in the
:link reftype=hd refid=UPconcepts.General concepts:elink.
section.
:p.
To the right of the VRWDN code, some entries have one or more of the
following codes.
:dl.
:dt.   +
:dd.This directory is collapsed, i.e. its subdirectories (if any)
are not at present displayed on the screen.
:dt.   *
:dd.This entry is a symbolic link or pseudo-directory.
:dt.   #
:dd.This entry describes a file rather than a directory.
:dt.   ?
:dd.There is no file or directory on the disk that matches this entry.
This might mean that you have made an error in the name; alternatively,
it might mean that you are specifying a directory or file that you
haven't yet created.
:edl.
:p.
To edit the tree, you have the following options.
:ul.
:li.You can navigate through the list of directories with mouse clicks, with the
cursor up/down keys, and also with the Home, End, PageUp, and
PageDown keys.
:li.To change a permission, type one of the characters V, R, W, D, or N, or
click on the buttons with these labels.
This toggles the state of the corresponding permission code for the
currently selected directory.
:li.The "-" key or button collapses a directory by removing its subdirectories
from the screen listing. (The subdirectories are still there, and are still affected
by things like the "Propagate" option.  They simply aren't shown on the screen.)
To get the subdirectories back, type the "+" key or button.
:li.The "Add child" button adds a new child node under the current node.
:li.The "Edit" button allows you to edit the details for the current entry.
(Instructions for doing this are given later in this page.) You can also
edit by double-clicking on the entry.
This option is disabled if the current entry describes a file or directory
that is physically present on the disk.
:li.The "Delete" button deletes the current node and all of its subtrees.
This option is disabled if the current entry describes a directory
that is physically present on the disk.
:li.The "Inherit" button gives the selected entry a copy of the
current permissions of its parent.
:li.The "Propagate" button copies the permissions of the currently selected directory
to all of its subdirectories. Use this if you want to change an entire
subtree in one operation.
:eul.
:p.
When you have finished editing the permissions, click on the "Done" button
to go back to the previous dialogue. If you have made a mistake, you still have
the option of using the "Cancel" button on that previous dialogue.

:p.:hp2.Modifying the details for one entry:ehp2.

:p.The "Edit" and "Add child" options will bring up a screen window that
describes one tree node. The radio buttons at the top let you specify
one of three kinds of node.
:dl break=all.
:dt.    subdirectory or file
:dd.This refers to a subdirectory or file that is contained within the
parent directory. The "Name" field gives the name of the subdirectory or file.
Note that you cannot choose this option for the top-level directory,
because the top-level directory has no parent.
:dt.    link
:dd.This creates a symbolic link. In the "Name" field, put a name of
your own choice; this will be the name of the directory as seen by the
client. In the "Path" field, put the full physical path (including
drive letter) for this directory or file.
:dt.    pseudo-directory
:dd.This is a special case of a link, where there is no physical path.
A pseudo-directory should contain only links and other pseudo-directories.
The most common use for a pseudo-directory is as the top-level
directory for a user, containing links to the drives and/or directories
that that user will be allowed to see.
:edl.

:p.:hp2.Setting permissions for individual files:ehp2.

:p.The access permissions used by FtpServer are normally given to directories,
and the permissions for a directory apply to all non-directory files in that
directory. However, Setup will let you define an entry for a non-directory
file, and give it access permissions. (It would be tedious to do this for
every file, but this feature can be used for special cases.) This gives you
a method for making the permissions for a file different from the permissions
of the directory that it is in.

:p.If you create a link or pseudo-directory with the same name as a real
file or subdirectory, your link will take precedence over the real name.
In effect, the real file or subdirectory will become invisible as far as
the user is concerned.

.*************************************************************************
.*   INI and TNI FILES
.***********************************

:h1 id=tnifiles.TNI files

:hp2.INI and TNI files:ehp2.

:p.The various configuration options and user details for FtpServer are kept
in INI files. The Setup utility is, in effect, an editor for the main INI
file FTPD.INI. You might notice that there are also files called Setup.INI
and Monitor.INI. Those are to hold details like fonts and window positions
that are not needed by the FTP daemon but are needed by the associated
utilities.

:p.The native implementation of OS/2 INI files is efficient, but for some
people it has created reliability problems. The amount of space that OS/2
allocates for shared memory has not grown to match the growth in real
memory sizes. When you run out of shared memory, one of the symptoms is
INI file corruption.

:p.To get around this problem, FtpServer offers the option of using TNI
(text-mode INI) files as a replacement for INI files. A TNI file contains
the same information as an INI file, but in human-readable form.

:p.The four programs Ftpd.EXE, Setup.EXE, Monitor.EXE, and VioSetup will use TNI
mode if the corresponding TNI file exists in the current directory and
there is no corresponding INI file. Conversely, they will use INI mode
if the INI file exists and the TNI file doesn't.
If both an INI and a TNI file exist in that directory, Ftpd.exe and Setup.exe
remember what was specified the last time that Setup was run with an
explicit "T" or "I" parameter. Similarly, Monitor.exe remembers the last
time it was run with an explicit "T" or "I" parameter.

:p.The above rule can be overridden with a command-line parameter.
If you start any of these programs with the "T" parameter, that is
:xmp.
                ftpd T
                setup -T
                monitor -T
                viosetup T
:exmp.
then those programs use the files FTPD.TNI, SETUP.TNI, and MONITOR.TNI
instead of the corresponding INI files. The parameter is case-insensitive,
so a "t" parameter will do the same thing. For the opposite decision,
you can specify the "I" parameter.

:p.Ftpd.EXE has a command-line parameter "S" that means the same as "T".
This used to be needed because of an ambiguity, but that parameter will
eventually be phased out.

:p.In practice is usually easier to run Setup initially with an
explicit "T" or "I" parameter, because that is a decision that will be remembered
by all of Ftpd and Setup and VioSetup. From then on, the command-line parameter is not
needed unless you change your mind.

:p.Note that Ftpd.EXE also has an optional command-line parameter to
specify the use of a different file name for the INI or TNI file. (Usually because
you want to give an explicit directory specification.) If that file name
ends in ".TNI" or ".INI", that will control whether TNI or INI mode
will be chosen.

:p.If you use TNI files, it can be useful to have utilities that will
convert between the INI and TNI formats. Those utilities are available in
the GenINI package (freeware), which you can download from
http&colon.//www.pmoylan.org/pages/os2/genini.html.

:p.If you are uncertain whether you are using INI or TNI format, both
Setup and VioSetup display this on the screen. For Ftpd.exe, the indication
is an "[I]" or "[T]" at the top left of the screen.

.*************************************************************************
.*   REMOTE CONFIGURATION
.***********************************

:h1 id=remoteconfig.Remote configuration

:p.Setup also offers the option of remote setup. That
is, you can run Setup on one computer and use it to configure a copy
of FtpServer that is installed on a different computer. To do this, you have
to have the freeware utility INIServe running on the same computer as
FtpServer. You can find INIServe at http&colon.&slash.&slash.www.pmoylan.org&slash.pages&slash.os2.

:p.If you select the "Remote" radio button after starting Setup, a "Setup"
pushbutton is enabled. Clicking on this gives you four fields to fill in&colon.

:dl break=all.
:dt.    Hostname
:dd.The name (or IP address) of the machine on which FtpServer is running.
:dt.    INIServe port
:dd.The TCP port that INIServe has been configured to listen on. The default
value is 3560.
:dt.    INIServe password
:dd.The password needed to log in to your copy of INIServe.
:dt.    FtpServer directory
:dd.The full path name of the directory, on the remote machine, where FTPD.INI
is located.
:edl.

:p.When you close the Setup window, you can click on the "GO" button to connect
to the remote machine. If this gives a "failed to connect" or similar error
message, it probably means that you don't have INIServe running on the
remote machine, or that you have done something like specifying an incorrect
port number.

:p.Once the connection is made, the operation is the same as for the
case of local configuration.

:p.When doing remote configuration, the decision as to whether to use Setup.INI or Setup.TNI
is based on local files, because you are still using a local copy of Setup. On
the other hand, the choice between FTPD.INI and FTPD.TNI is based on those files,
if they exist, at the remote location. This is because we want to make the same choice as
will be made by the remote FTPD.EXE.

.***********************************
.*   THE VIOSETUP UTILITY
.***********************************

:h1 id=configuser.The VIOSetup utility
:hp2.The VIOSetup utility:ehp2.
:p.
The program VIOSETUP.EXE has three functions:
:ul compact.
:li.To set the parameters that the server will use when it starts up.
:li.To place controls on which IP addresses may access the server.
:li.To create and edit user permissions.
:eul.

:p.
Use the F4 and F5 function keys on the keyboard to toggle among these functions.

:note.VIOSetup is an older version of Setup, and because of this it does
not support some of the FtpServer options. It is still included in the
distribution, because some people prefer text-mode programs, but it is
no longer being updated.
:p.
The parameter settings are stored in a file FTPD.INI or FTPD.TNI. You can
use a command-line parameter 'T' or 'I' (without the quotation marks) to
choose which of those you are using. With no command-line parameter, the
choice is governed by the last time you ran Setup (not VIOSetup) with an
explicit 'T' or 'I' parameter.

:p.Some of the changes
you make will take effect immediately, while others will not take effect until
the next time you start the server. The precise rules are given in the
description of the Setup program.
:p.
In particular, the user permissions are read each time a user attempts
to log in. You may therefore alter the user permissions while the server
is running, and the alterations will affect the next user to log in.

:p.Similarly, when you change the security settings the change
takes effect immediately. You do not need to restart the server.

:p.Because VIOSetup is used by relatively few people, it is not updated as
often as the GUI Setup program is. Depending on which version of FtpServer
you have, you will often find that there are options supported by
Setup but not by VIOSetup.

:p.
Now read

:ul compact.
:li.:link reftype=hd refid=configuser1.Setting the server parameters:elink.
:li.:link reftype=hd refid=security.Security settings:elink.
:li.:link reftype=hd refid=configuser1a.More setup options:elink.
:li.:link reftype=hd refid=configuser2.Modifying user permissions:elink.
:eul.

.***********************************
.*   SETTING THE SERVER PARAMETERS
.***********************************

:h2 id=configuser1.Setting the server parameters
:hp2.Setting the server parameters:ehp2.
:p.
When you run VIOSETUP.EXE, you get a screen showing the following items.

:dl break=all.
:dt.Server port
:dd.Unless you are doing something nonstandard (for example, running two ftp servers
on the same machine) this should always be 21.
:dt.Maximum number of users
:dd.This specifies how many clients will be allowed to use the server simultaneously.
I usually set this to 10. Higher values will, of course, increase the load on your
processor.
:p.Note: this is a global maximum. You may also set this to a high value, and then
control the number of users on a per-username basis.
:dt.Maximum number of guest users
:dd.This typically should be slightly less than the number specified for the maximum
number of users, to reserve one or more login slots for the system manager and other
non-guest users.
:dt.Free space threshold (MB)
:dd.This specifies the amount of free space that must be available on a drive for
uploads to be enabled. If the free space, in megabytes, falls below this level then
uploads will be disabled.
:dt.Timeout (seconds)
:dd.The time that a client session may remain idle before the user is evicted.
You will find that many ftp clients, especially web browsers, don't log out properly,
so their sessions have to be killed with the timeout mechanism.
:dt.Transfer log format
:dd.In addition to the detailed transaction log, you can ask the server to
produce a user log (FTPUSERS.LOG) and/or a log in common log format (COMMON.LOG).
(If you delete these files, they will be re-created. It would be a good idea to
delete them periodically, or move them to an archive, so that they do not grow
too large.) The user log produces a list of files that have been uploaded
or downloaded. The common log contains similar information, but in a format
used by many http servers. This allows you to use log analysis tools that
have been designed for web servers.
:dt.Transfer logging level
:dd.The logging level controls how much detail gets written to the user log.
:dl.
:dt.     0
:dd.No logging
:dt.     1
:dd.Log successful file transfers
:dt.     2
:dd.Log successful and unsuccessful file transfers
:dt.     3
:dd.Log all users, even those who didn't transfer any files
:edl.
:dt.Transaction logging
:dd.The transaction log is a much more detailed log. You can choose to send it
to the screen, or to a disk file, or both.
The disk file is called FTPTRANS.LOG, and it is updated approximately once every
minute if this feature is enabled.
:p.Warning: Transaction logging can create very large log files. I suggest that
you don't enable this feature unless you are trying to track down a problem.
:edl.

:p.
To modify any of these parameters, use the up/down arrow keys to get to the
desired item, then type in the new value. (The backspace, Insert, Delete, Home,
and End keys
will also work during editing.) The new value is accepted when you type the
Enter key, or when you use the function keys to go to another field.
:p.
When you have finished editing, use the Esc key to exit from the VIOSetup program, or
type F5 to get to the :link reftype=hd refid=security.security screen:elink..

.***********************************
.*   SECURITY SETTINGS
.***********************************

:h2 id=security.Security settings
:hp2.Security settings:ehp2.
:p.
To modify the security settings, run VIOSETUP.EXE, and then type the F5 function key
on the keyboard to get to the "Security" screen page.
:p.At the top of this page there is a field called the "Same IP limit". This specifies the
maximum number of users that can be connected simultaneously from the same IP
address. It is primarily a protection against users who hog the server by
logging in more than once.
:p.Set this value to whatever you want, finishing with the "Enter" or "cursor down"
key to confirm the new value. The "cursor down" key will take you to the IP address
controls, as described below. When you have finished setting the values on this
page, type F5 to get to
:link reftype=hd refid=configuser1a.more setup options:elink..
:p.
:hp2.Restricting access to certain IP addresses:ehp2.
:p.
The large box on this screen page defines a filter for IP addresses. This is for
putting restrictions on which remote hosts are allowed to log into the server.
(If you don't need this feature, just use a single "Allow all" entry.) When a
client tries to connect, the server searches this list, starting at the
beginning, for the first entry that matches the client's IP address. There will
always be a match, because the last entry is always an "everything else" entry.
The allow/refuse flag on the matching entry is used to decide whether the
client should be allowed to connect. If the flag value is "refuse", the
connection attempt is rejected.

:p.Each list entry has an allow/refuse flag and two numeric components, an address
and a mask. Each of these is expressed in "dotted quad" notation: a four-byte value
where each byte has its value
written out in decimal. (This is a standard convention for writing IP addresses.)
A client address matches an entry if
:p.       (client IP address) :hp2.AND:ehp2. mask = (IP address in the list)
:p.where :hp2.AND:ehp2. means the bit-by-bit Boolean "logical AND" operation.
:p.Note, in particular, the two extreme cases:
:ul.
:li.If the mask is 255.255.255.255, then we are specifying an exact match between
the client IP address and the address in the list.
:li.If the mask is 0.0.0.0, then any IP address will match this entry. You can use
this to specify an "everything else" condition.
:eul.
:p.
:p.:hp2.Example 1.:ehp2. If you want to lock out all machines with IP address in
the range 123.45.67.0 to 123.45.67.127, your list would look like this.
:sl compact.
:li.Refuse   123.45.67.0   255.255.255.128
:li.Allow    all others
:esl.

:p.:hp2.Example 2.:ehp2. Suppose you want to give access only to your local network,
which has addresses in the range 123.45.66.0 to 123.45.67.255. You can do this as
follows.
:sl compact.
:li.Allow    123.45.66.0   255.255.254.0
:li.Refuse   all others
:esl.

:p.:hp2.Example 3.:ehp2. To allow access to 123.45.67.89, but to lock out everyone
else in 123.45.67.*, you can use the rules
:sl compact.
:li.Allow    123.45.67.89   255.255.255.255
:li.Refuse   123.45.67.0    255.255.255.0
:li.Allow    all others
:esl.

:p.Notice that the list always finishes with an "all others" entry. The VIOSetup
program will allow you to change the allow/refuse flag on this final entry, but
it will not allow you to delete it.

.***********************************
.*   MORE VIOSETUP OPTIONS
.***********************************

:h2 id=configuser1a.More setup options
:hp2.More setup options:ehp2.

:p.In the present version, the only option on this page is a choice between
binding to all interfaces, or binding to a specific IP address. Use the
cursor left/right keys to highlight the option you want. If you highlight
"specific address", you will be given the opportunity to edit the address.
After editing it, use the Enter key or a cursor up/down key to confirm that
you have finished editing. When you have finished setting the values on this
page, type F5 to get to the
:link reftype=hd refid=configuser2.user permission editor:elink..

:p.For most applications, the best choice is "all interfaces". With this
choice the server listens for ftp requests on all your network interfaces,
even if your machine has multiple IP addresses.

:p.The "specific address" option is for the case where you have two or more
IP addresses, but you want the server to respond to only one of them. In
this case you could, if you wished, run several independent ftp servers on
the same machine, each responding to a different address.

:p.(Another way to run several ftp servers is to make each one listen on
a different port. That's a less attractive option, however, because most
ftp clients expect to find the server on the standard port 21.)

:p.If you do run multiple instances of ftpd.exe, start each of them from a
different directory. This is because ftpd.exe expects to find its INI file
in the working directory, and for multiple copies you
would want to have a different INI file for each one.

.***********************************
.*   MODIFYING USER PERMISSIONS
.***********************************

:h2 id=configuser2.Modifying user permissions
:hp2.Modifying user permissions:ehp2.
:p.
To modify the user permissions, run VIOSETUP.EXE, and then type the F4 function key
- or type F5 three times - to get to the "Users" screen page. This will give you a list of
all existing users. The first time you run the program, the list will probably
be empty.
:p.
From this screen, you can add, delete, or modify users. When you have finished,
use the F4 or F5 function key to get to the other setup screens, or type X to
exit from the VIOSetup program.
:p.
:hp2.Deleting a user:ehp2.
:p.
Use the up/down arrow keys to get to the user you want to delete, and type
the Del (delete) key.
:p.
:hp2.Adding a new user:ehp2.
:p.
Type A, and then proceed as for :link reftype=hd refid=edituser.Editing a user's permissions:elink..
:p.
:hp2.Cloning an existing user:ehp2.
:p.
Type C, and then proceed as for :link reftype=hd refid=edituser.Editing a user's permissions:elink..
This is the same as adding a new user, except that the new user's attributes are copied from
those for the user that was selected when the C command was typed.
:p.
:hp2.Editing the permissions of an existing user:ehp2.
:p.
Type E, and then follow the instructions in the section :link reftype=hd refid=edituser.Editing a user's permissions:elink..

.***********************************
.*   EDITING A USER'S PERMISSIONS
.***********************************

:h2 id=edituser.Editing a user's permissions
:hp2.Editing a user's permissions:ehp2.
:p.
You get to this point by running the VIOSetup program, typing F4 or F5 to get to
the user editor, and then using one of the "A" (add user) or "E" (edit user)
options.
:p.
By now you should have six fields near the top of the screen.
:dl break=all.
:dt.    User name
:dd.The name that the user will use when logging in.
:dt.    Real name
:dd.This field is not used by FtpServer; it is purely for your own records.
:dt.    Category
:dd.One of User, Guest, NoPassword, or Manager.
:dt.    Password
:dd.This user's password.
:dt.    User limit
:dd.The maximum number of simultaneous sessions with this user name.
(If you don't want such a control, just make this number larger than the
global user limit.)
:dt.    Speed limit
:dd.An approximate upper bound on the file transfer speed for this user,
in case you want to restrict how much of the processor power this user
can get. If you don't want such a control, just make this a large
number.
:dt.    Suppress log
:dd.If you specify "Yes" for this option then the operations performed
by this user will not be logged.
:edl.
:p.
Use the up/down arrow keys to get to the field you want to edit, and
then modify it as necessary. For the "Category" and "Suppress log" fields, use the
left/right arrow keys to select the value you want.

:p.Warning: If you change the user name, the permissions for the previous
user name will be deleted. You should also avoid using a user name
that is the same as for some other user.

:p.The bottom half of this screen gives a summary - but not a complete
description - of the top level of this user's directory tree. To see
the complete details, and to modify those details, use the "cursor down"
key to move to this part of the screen.

:p.Instructions for modifying the user's tree are on the
:link reftype=hd refid=edittree.next page:elink. of this document.

:p.:hp2.Converting from older formats:ehp2.
:p.Some details of the format of user information in FTPD.INI were
changed in version 0.71 of FtpServer. Both the old and new formats are
supported in versions 0.71 up to 0.80, but in later versions the old format will
probably no longer be accepted. Thus, you should convert all your user permission
data to the new format.
:p.
The VIOSetup program will automatically perform the conversion each time
you edit a user. If you have a small number of users defined, then the
way to do the conversion is to run VIOSetup and edit each user (without
necessarily making any changes).
:p.
If you have a large number of users, it is easier to use the LoadPRM
program to do the conversion. This can be done with the following
sequence of commands.
:xmp.
        storeprm *
        loadprm *
        del *.prm
:exmp.
:p.(The final deletion can be omitted if you prefer to keep a copy of
the PRM files. If ever your INI file is damaged or destroyed, you can
use the LoadPRM utility to re-load user data from PRM files.)

.***********************************
.*   EDITING A DIRECTORY TREE
.***********************************

:h2 id=edittree.Editing a directory tree
:hp2.Editing a directory tree:ehp2.

:p.You get to this point while
:link reftype=hd refid=edituser.editing a user's permissions:elink..
The picture that is shown on the screen is a representation of the
directory tree for this user. Initially it will show enough subtrees
to reveal all symbolic links, and all entries for which the user's
permissions are different from the parent node's permissions. (For
a new user, there will be nothing except an empty root node.) In
the course of editing this tree you can expand or collapse nodes to
control how much detail is shown on the screen.
:p.
The top of the screen shows both a physical path and a virtual path
for the current entry. The physical path is the true location of
the file or directory on your disk. The virtual path is the path
as the client sees it.
:p.
Each entry describes one directory or file. At the left of each line,
you will see a code consisting of one or more of the letters "VRWDN".
The meanings of these user permission codes are explained in the
:link reftype=hd refid=UPconcepts.General concepts:elink.
section.
:p.
To the right of the VRWDN code, some entries have one or more of the
following codes.
:dl.
:dt.   +
:dd.This directory is collapsed, i.e. its subdirectories (if any)
are not at present displayed on the screen.
:dt.   *
:dd.This entry is a symbolic link.
:dt.   #
:dd.This entry describes a file rather than a directory.
:dt.   ?
:dd.There is no file or directory on the disk that matches this entry.
This might mean that you have made an error in the name; alternatively,
it might mean that you are specifying a directory or file that you
haven't yet created.
:edl.
:p.
To edit the tree, you have the following options.
:ul.
:li.You can navigate through the list of directories with the
cursor up/down keys, and also with the Home, End, PageUp, and
PageDown keys.
:li.To change a permission, type one of the characters V, R, W, D, or N.
This toggles the state of the corresponding permission code for the
currently selected directory.
:li.Typing the "I" key gives the selected entry a copy of the
current permissions of its parent.
:li.The "P" key copies the permissions of the currently selected directory
to all of its subdirectories. Use this if you want to change an entire
subtree in one operation.
:li.The "-" key collapses a directory by removing its subdirectories
from the screen listing. (But these subdirectories will still be affected
by the "P" option.) To get the subdirectories back, type the "+" key.
:li.The "A" key adds a new child node under the current node.
:li.The "Del" key deletes the current node and all of its subtrees.
This option is disabled if the current entry describes a file or directory
that is physically present on the disk.
:li.The "E" key allows you to edit the details for the current entry.
(Instructions for doing this are given later in this page.)
This option is disabled if the current entry describes a file or directory
that is physically present on the disk.
:eul.
:p.
When you have finished editing the permissions, type B to go back to
the previous screen, or X to exit completely from VIOSetup.

:p.:hp2.Modifying the details for one entry:ehp2.

:p.The "E" or "A" command will bring up a screen window with three
details that you can modify. Use the cursor up/down keys to go from
one field to another.
:ol.
:li.The Name field gives the subdirectory or file name.
:li.The Link field should have value "no" for an ordinary subdirectory
or file, and "yes" for a symbolic link. Use the cursor left/right keys
to change the value of this field.
:li.The Path is the physical path (including drive letter) for this
directory or file. You can modify this only if the Link field is set
to "yes". If you leave the Path blank, you are defining a pseudo-directory.
:eol.
:p.When you have finished editing these details, type the Esc key to
return to the tree.

:p.:hp2.Setting permissions for individual files:ehp2.

:p.The access permissions used by FtpServer are normally given to directories,
and the permissions for a directory apply to all non-directory files in that
directory. However, VIOSetup will let you define an entry for a non-directory
file, and give it access permissions. (It would be tedious to do this for
every file, but this feature can be used for special cases.) This gives you
a method for making the permissions for a file different from the permissions
of the directory that it is in.

.***********************************
.*   RUNNING THE SERVER
.***********************************

:h1.Running the server
:hp2.Running the server:ehp2.
:p.
The server executable is called FTPD.EXE. You can run this the way you run
any other OS/2 program: from the command-line, by clicking on an icon, from
the Startup folder, etc. If you are running several server applications, then
one good choice is to put a command to start the server in the
command file \TCPIP\BIN\TCPEXIT.CMD. My personal preference is to have a program
object in the Startup folder, because that's an easy way to specify the
working directory; but that's only available if you run the Workplace Shell
as your command shell, and some people running servers prefer to have a
non-graphical command shell.
:p.
Normally the server takes its configurable parameters from the INI or TNI file
created by the :link reftype=hd refid=PMconfiguser.Setup program:elink.. You may, however,
override these parameters by specifying :link reftype=hd refid=parameters.command-line parameters:elink..
:p.
You also have the options of :link reftype=hd refid=inetd.running the server from inetd:elink.
or :link reftype=hd refid=detached.running the server as a detached program:elink..
:p.
If the server is running as a non-detached program, you can close it with one or two
CTRL/C characters from the keyboard. (The 'G' and 'Q' commands, used for shutdown
in earlier versions, are no longer supported.) The first CTRL/C requests a
gradual shutdown: no new users are accepted, but transfers currently in progress
are allowed to complete (unless they time out). If you don't want to wait for this,
type a second CTRL/C. The program will then close down even if there are logged-in users.

:p.Two other ways of shutting down the server are
:ul.
:li.Using the :link reftype=hd refid=SITE.SITE MNGR EXIT:elink. and
:link reftype=hd refid=SITE.SITE MNGR GXIT:elink. commands. The
easiest way to invoke these is with the :link reftype=hd refid=Monitor.Monitor:elink.
utility.
:li.By signalling on the global event semaphore \SEM32\FTPSERVER\SHUTDOWN.
The easiest way to do this is by running the script SHUTFTPD.CMD, which
you will find in the 'Tools' directory.

:eul.

:p.If you need to know when the program has finished shutting down, you can
wait on the global event semaphore \SEM32\FTPSERVER\FINISHED.

.***********************************
.*   COMMAND LINE PARAMETERS
.***********************************

:h2 id=parameters.Command line parameters
:hp2.Command line parameters:ehp2.
:p.
Normally you don't need any parameters when invoking FTPD.EXE, because the server
takes its parameters from the INI or TNI file. (And the contents of that file are
controlled by the Setup program.) You may, however, override the parameters in
the INI or TNI file by giving command-line parameters.
:p.
There are several optional parameters. For the "I" option, the parameter is
the letter "I" followed by a file name (which may not contain a space character.)
For the "S" and "X" options, the parameter is the single letter "S" or "X".
For all other options, the parameter specification is a letter followed
by a (decimal) number. The parameters may be given in any order.
:dl.
:dt.   D
:dd.Detailed transaction logging (default 0).
:dl compact.
:dt.         0
:dd.No logging
:dt.         1
:dd.Logging to the transaction log on disk
:dt.         2
:dd.Logging to the screen
:dt.         3
:dd.Logging to both the screen and the disk file
:edl.

:p.Note that this option specifies how much logging is done, but it
does not control the name or location of the log file. The file name
and path continue to be as specified on the
:link reftype=hd refid=PMLogging.Logging:elink. page of the Setup program.

:dt.   F
:dd.Free space threshold (megabytes). Users can't upload to a
drive that has less than this amount of free space available.
The default is 10.

:dt.   G
:dd.Maximum number of guest users. If you make this smaller than
the value for M (see below), you effectively reserve some
slots for non-guest users. The default is M-1.

:dt.   I
:dd.(not followed by a file name)
:p.Forces the use of INI rather than TNI mode.

:dt.   I
:dd.(followed by a file name)
:p.INI or TNI file name. Normally you don't need to specify this, because
the default name is FTPD.INI, in the current working directory.
You may specify this parameter if, for example, you want the INI file
to be in a different directory. Note: if the file name ends in ".TNI",
this automatically forces TNI mode, as if the 'T' option had
been used.

:dt.   L
:dd.User logging option (default 1).
:dl compact.
:dt.         0
:dd.No logging
:dt.         1
:dd.Logging of successful file transfers
:dt.         2
:dd.Logging of successful and unsuccessful file transfers
:dt.         3
:dd.Logging of all users
:edl.

:p.Note that this option controls how much detail is logged, but it
does not override the other information specified on the
:link reftype=hd refid=PMLogging.Logging:elink. page of the Setup program.
That is, options like the name of the log file(s) continue to be
taken from the INI file.

:dt.   M
:dd.Maximum number of simultaneous users. To limit the number
to 12, for example, use the command
.br
             ftpd m12
.br
The default is 10.

:dt.   P
:dd.The server's port number.  To make the server listen on port 5003,
for example, you start the program with the command
.br
             ftpd p5003
.br
The default port number is 21.

:dt.   S
:dd.Operate on the text-mode file FTPD.TNI, rather than on the
file FTPD.INI, as explained in the section on
:link reftype=hd refid=tnifiles.TNI files:elink.. This option
will eventually be phased out, as it means the same as T.

:dt.   T
:dd.(not followed by a number)
:p.Operate on the text-mode file FTPD.TNI, rather than on the
file FTPD.INI, as explained in the section on
:link reftype=hd refid=tnifiles.TNI files:elink..

:dt.   T
:dd.(followed by a number)
:p.Timeout limit, i.e. the time before an inactive user is forcibly
removed. The value is in seconds, and the default is 900.

:dt.   X
:dd.Extra detail in the transaction log. Since this option is
intended for debugging, its effect may vary from one version of
FtpServer to the next.

:edl.

:p.Note that the I and T parameters have different meanings depending
on whether the letter I or T is followed by a value. If necessary you
can use these twice to get the two different meanings.

.***********************************
.*   RUNNING FROM INETD
.***********************************

:h2 id=inetd.Running from inetd
:hp2.Running the server from inetd:ehp2.
:p.
Inetd, which is part of the Warp 4 distribution, is a "listener"
program that can intercept incoming connection attempts, and
start up a server when needed.
:p.
The advantage is that FtpServer doesn't actually get loaded into
main memory until a client wants to connect. Thus, it might be
a good option if you expect clients to connect only occasionally.
:p.
The disadvantage is that a separate copy of the server is
started for each logged-in user. This makes inetd a bad choice
if you expect lots of connections.
:p.
If you want to run FtpServer from inetd, the way to do it is
as follows:
:ol.
:li.Ensure that inetd will be run the next time you boot.
The usual way of doing this is to include the line
.br
           start /min inetd
.br
in your TCPSTART.CMD, and to invoke TCPSTART.CMD from
your startup folder. TCPSTART.CMD may be found in the
directory \tcpip\bin.

:li.Edit the file \mptn\etc\inetd.lst so that it contains the line
.br
     ftp tcp start /C /min d&colon.\Apps2\FtpServer\ftpd.exe
.br
(adjusting the path so that it refers to the directory
where you have installed FtpServer).
:eol.

:note.In early releases the inetd users had to use a file called ftpd.cmd.
That command file is now obsolete.
:p.
You may also include parameters on the inetd.lst line
that invokes ftpd.exe, subject to the following conditions:
:ol.
:li.The M parameter is useless, because in this mode of
operation the program is handling exactly one user.
:li.The P parameter, if present, will be ignored. When
running from inetd, you don't get a choice of ports.
:eol.

:p.In principle you can now start inetd. In practice I've found
that inetd doesn't release ports reliably, so if you already
have inetd running you'll probably have to re-boot.
:p.
Remark: I'm starting to suspect that inetd adds more overhead
than it saves, so I've reverted to not using it on my own
machine.

.***********************************
.*   RUNNING FTPSERVER DETACHED
.***********************************

:h2 id=detached.Running FtpServer detached
:hp2.Running the server as a detached program:ehp2.

:p.If you want to run the server detached, the appropriate command is
.br
       DETACH FTPD
.br
(with parameters, if desired). Note that a detached program
does not have any way of doing screen output or keyboard input,
so you can't get any screen messages in this case. Nor can you use
CTRL/C to stop the program.

:p.Although you can't shut down the server from the keyboard in this
case, you can still shut it down by using the
:link reftype=hd refid=SITE.SITE MNGR:elink. commands, or with
the SHUTFTPD.CMD utility.

.***********************************
.*   WELCOME MESSAGES
.***********************************

:h2 id=welcome.Welcome messages
:hp2.Welcome messages:ehp2.

:p.If you want to give a message to users when they log in, put a
plain text file called WELCOME.MSG or WELCOME0.MSG in the
same directory as ftpd.exe.
:ul compact.
:li.WELCOME0.MSG, if present, is displayed to the user when the initial connection
is made.
:li.WELCOME.MSG, if present, is displayed to the user after the username and
password have been accepted.
:eul.
:p.You can use both of these options together, if you wish, but it would
probably be less confusing to the users if you had only one message.

:p.You can also put a text file called DIR.MSG in any user directory.
Users will get this message the first time they go to that directory.

:p.There is a limited form of macro expansion available in these message
files. The following macros may be included.
:dl break=fit.
:dt.    %a
:dd.The client's IP address.
:dt.    %A
:dd.The client's hostname.
:dt.    %i"filename"
:dd.Includes the contents of the given file in the message. Nesting is
permitted - that is, the included file may also contain a %i macro - but
is limited to a depth of 5 in order to guard against unlimited recursion. In the
case of WELCOME.MSG or WELCOME0.MSG the filename may be given as an
absolute pathname, but if relative then it is relative to the directory
in which ftpd.exe is running. (Note that you should never give
non-trusted users access to this directory.) In the case of DIR.MSG, the filename
is interpreted in the way the client sees the file system - i.e. if
relative then it is relative to the client's current directory - and
the client must have permission to read this file.
:dt.    %m
:dd.Expands to a character string giving the maximum allowed number of users
with the current username.
:dt.    %M
:dd.Expands to a character string giving the global maximum allowed number of users.
:dt.    %t
:dd.Expands to a string giving the local time.
:dt.    %T
:dd.For now, this is the same as %t. This is a deprecated use, because in future %T might have a different meaning.
:dt.    %u
:dd.Expands to a string giving a user number within this user's group.
:dt.    %U
:dd.Expands to a string giving this user's global user number.
:dt.    %v
:dd.Expands to a string giving the current FtpServer version.
:dt.    %%
:dd.The '%' character.
:edl.

.***********************************
.*   THE SITE COMMANDS
.***********************************

:h2 id=SITE.The SITE commands
:hp2.The SITE commands:ehp2.

:p.:hp2.The SITE PERM command:ehp2.

:p.The command SITE PERM returns a three-character string showing whether
you have read, write, and/or delete permission for the current directory.
This command was added while I was testing a new feature. It might be
withdrawn in future versions, because it is not particularly useful for
most users.

:p.:hp2.The SITE MNGR commands:ehp2.

:p.Commands in this group may be used only from a manager account.
Currently the following options are available.

:dl break=all.
:dt.SITE MNGR EXEC
:dd.Runs another program. See below for further details.
:dt.SITE MNGR EXIT
:dd.Shuts down the server. (Don't do this unless you really mean it!)
:dt.SITE MNGR GXIT
:dd.Shuts down the server after completing the transfers that are
currently in progress - i.e. the same action as for the keyboard G command.
:dt.SITE MNGR KILL nnn
:dd.Forcibly logs out user number nnn. The number must match the one
returned by the SITE MNGR LIST command.
:dt.SITE MNGR LIST
:dd.Returns a list of currently logged-in users. The main purpose of this
command is to support the Monitor utility.
:edl.

:p.The SITE MNGR EXEC command starts a new program in a separate session.
(If FtpServer is running detached, then the new program must also run
detached.) For example, you could zip up the user log with the command

:xmp.      site mngr exec zip.exe today.zip ftpusers.log
:exmp.

:p.The mechanism used to implement this feature is to run the specified
command from a CMD.EXE shell, whose working directory has been set to
the user's current directory. (Unless the user's directory is a
pseudo-directory, in which case the working directory is the same
as FtpServer's working directory.) This means that you can specify any
command that is legal in CMD.EXE, for example

:xmp.      site mngr exec dir >dirlist.txt
:exmp.

:p.Note that redirection of the output was necessary in this example;
otherwise, you wouldn't have any way of seeing the results. In all
cases the server does not reply to the SITE command until the
spawned command shell has terminated. You could, however, start an
asynchronous operation by using the shell command START.

:p.Running OS/2 command files and Rexx programs is legal,
as in the following example.

:xmp.      site mngr exec test.cmd parameter1 parameter2
:exmp.

:p.:hp2.The SITE UTIME command:ehp2.

:p.This is a nonstandard command, available on some ftp clients, for
setting the timestamps on an uploaded file. An example of its use is

:xmp.       site utime my file.exe 20040925210005 20040925210005 20040925210005 UTC
:exmp.

:p.which sets the timestamps for the file called "my file.exe". The three
numeric parameters are for the time last modified, the time last
accessed, and the time created, in the format YYYYMMDDHHMMSS. (Year,
month, day, hour, minute, second.) The optional final "UTC" specifies
that these dates and times are expressed relative to the standard time
at longitude zero. If the "UTC" is absent then the time is instead taken
to mean local time at the server.

:p.FtpServer is tolerant of the case where only one timestamp is
provided; in that case, it assumes that all three timestamps are
intended to be equal. Note, however, that providing fewer than three
timestamp parameters can create an ambiguity, because there is nothing in
the syntax to show where the file name ends and the first timestamp starts.
This ambiguity cannot be fixed, because the SITE UTIME command was
apparently designed for an operating system that did not support spaces
in file names.

:p.Note that this command will work only if the client has write
permission for the file whose timestamp is being set.

.***********************************
.*   THE MONITOR UTILITY
.***********************************

:h1 id=Monitor.The Monitor utility
:hp2.The Monitor utility:ehp2.
:p.
The program MONITOR.EXE allows the system manager to see who is currently
logged in, and to kill sessions where necessary. This program can be run
either on the same machine as the server, or remotely.

:note.The monitor does not give useful information in the case where
the server is run from inetd, because in that case a separate copy
of the server is spawned for each client.

:p.There are two optional parameters. The first is the "-T" option, which
makes the program take its configuration data from MONITOR.TNI rather than
from MONITOR.INI. This is explained in the section on
:link reftype=hd refid=tnifiles.TNI files:elink..

The second is the name of this program's
INI file. Normally you don't have to specify this, because the name
MONITOR.INI is assumed by default. This option exists for the case
where you want to run multiple copies of the monitor to monitor
different servers, or to use a different font or screen size/position.
Note that specifying an explicit file name overrides the "-T" option.
The program will use a TNI file if the file name ends in ".TNI", and
an INI file otherwise.

:p.
When you start the program, it attempts to connect to the server. If it
fails to establish a connection, this might mean that the server is not
running. Alternatively, it might mean that you are attempting to connect
to the wrong machine, or to the right machine with the wrong manager account.
In the latter case, see the setup instructions below.

:p.Once the program has made a connection to the server, it displays one
line per client session. This shows the time the session started, the
IP address of the client, and the username. Once you select one item of
this display, you will also see the last command issued, and in some
cases a display of how many bytes have been transferred in the current
operation.
:p.
Clicking on the "kill user" button will terminate that client session
and forcibly log out the user.  Note that you can, with the aid of
shift/click or ctrl/click mouse operations, or the equivalent keyboard
operations, select several lines in the listbox, in which case "kill user"
will kill all of those sessions.
:p.
The "kill server" button allows you to shut down the server. (Don't do
this unless you really mean it.) You will be asked to confirm the shutdown
by selecting either "gradual shutdown" or "quick shutdown". The difference
is that the "gradual shutdown" allows file transfers already in progress
to complete before the server is shut down.

.**************************************
.*   SETTING UP THE MONITOR PARAMETERS
.**************************************

:p.:hp2.Setting up the Monitor parameters:ehp2.
:p.
Clicking on the setup button gives you a dialogue with the following items.
:dl break=all.
:dt.  Hostname
:dd.This specifies the machine on which the server is running, for example
mymachine.here.net.  If the machine has a fixed IP address, you can avoid a
nameserver lookup by specifying a numeric address, for example 123.45.67.89
:dt.  Port
:dd.This should normally be 21, but you might have set up the server to accept
connections from a non-standard port.
:dt.  Username
:dd.This must be the username for a manager account.
:dt.  Password
:dd.The password for the manager account.
:dt.  Update interval
:dd.The time (in seconds) between queries to the server to get the user
details. The smaller this value, the more load you are putting on the
server. A value between 5 and 10 seconds is usually a good compromise.
:edl.
:p.When you have finished filling in these details, close this window (or
type the Enter key) to return to the Monitor main display.

:p.:hp2.Capturing the client IP addresses:ehp2.

:p.If you want a list of selected IP addresses, for example to ban them
in your file, see the file GetClientAddrs.zip in the "Tools" directory.

.***********************************
.*   THE TMONITOR UTILITY
.***********************************

:h1.The TMonitor utility
:hp2.The TMonitor utility:ehp2.
:p.
TMONITOR.EXE is an older version of the monitor program. It is included
in this distribution for those who prefer to have a text-mode application.
:p.
When you start the program, it attempts to connect to the server. If it
fails to establish a connection, this might mean that the server is not
running. Alternatively, it might mean that you are attempting to connect
to the wrong machine, or to the right machine with the wrong manager account.
In the latter case, see the instructions below for setting up the TMonitor parameters.
:p.
To kill a client session, use the cursor up/down keys to get to the desired
session, and then type the K key.
:p.
To shut down the server, type Ctrl/K. (Hold down the Ctrl key while
typing K.) You will be asked to confirm the shutdown by typing either
G (for a gradual shutdown) or Q (for a quick shutdown).
:p.
To close the TMonitor program, type the X key.
:p.
:note text='Hint:'.If you want to use less screen space, issue the command
.br
         MODE CO80,10
.br
before running TMonitor.exe.

.**************************************
.*   SETTING UP THE TMONITOR PARAMETERS
.**************************************

:p.:hp2.Setting up the Monitor parameters:ehp2.
:p.
When running TMONITOR.EXE, typing S on the keyboard takes you to the setup screen.
There you will see four fields that have to be filled in.
:dl break=all.
:dt.  Server hostname
:dd.This specifies the machine on which the server is running, for example
mymachine.here.net. If the machine has a fixed IP address, you can avoid a
nameserver lookup by specifying a numeric address, for example 123.45.67.89
:dt.  Server port
:dd.This should normally be 21, but you might have set up the server to accept
connections from a non-standard port.
:dt.  User name
:dd.This must be the username for a manager account.
:dt.  Password
:dd.The password for the manager account.
:edl.
:p.When you have finished filling in these details, press the Esc key to return to
the TMonitor main screen.

.***************************************
.*   THE LOADPRM AND STOREPRM UTILITIES
.***************************************

:h1 id=loadstore.The LoadPRM and StorePRM utilities
:hp2.The LoadPRM utility:ehp2.
:p.
This utility is needed if you want to manually edit user permission files.
It copies information from a PRM file into the server's FTPD.INI. (Or into
the FTPDnnnn.INI files, if you have the "multiple INI files" option enabled.)
For example, the command
:xmp.       loadprm example
:exmp.
:p.takes the information in the file EXAMPLE.PRM and creates or updates an
entry in the INI file for a user called "example".

:p.Multiple wildcards are permitted: a '?' matches any single character, and
a '*' matches zero or more characters. (But only in the username part of the
parameter, not in the directory specification if present.) To load the information from :hp3.all:ehp3.
the PRM files in the current directory, use the command
:xmp.       loadprm *
:exmp.

:p.Optional -i or -t parameters specify the use of FTPD.INI (-i) or
FTPD.TNI (-t). If present, this parameter must come first.  For example,
:xmp.       loadprm -t abc\def\xy?u*v*
:exmp.
loads data from whatever PRM files in directory abc\def match the
pattern xy?u*v*, and puts the information into FTPD.TNI, or into
files with names FTPDnnnn.TNI if the "multiple INI files" option
is enabled in FTPD.TNI.

:p.If neither -i nor -t is specified (the usual case), the decision as
to whether to use INI or TNI format is based on the last time Setup was
run with an explicit -i or -t.

:p.You do not have to restart the server. The updated user information will
take effect the next time a user logs in.

:p.:hp2.The StorePRM utility:ehp2.
:p.
This utility creates a PRM file by copying the user information from
FTPD.INI or FTPD.TNI, as appropriate. You would use it if the INI file already contains user data
that you want to edit manually. For example, the command
:xmp.       storeprm example
:exmp.
:p.takes the information in the INI file for the user called "example",
and uses it to create a file EXAMPLE.PRM. (If EXAMPLE.PRM already exists,
the original copy is renamed EXAMPLE.BAK.)

:p.StorePRM has the same parameter options as LoadPRM.

.***************************************************
.*   THE REMOVE AND ONETIME AND LIMITEDUSE UTILITIES
.***************************************************

:h1.The REMOVE, ONETIME, and LIMITEDUSE utilities
:hp2.The REMOVE utility:ehp2.

:p.The file REMOVE.CMD is a Rexx script for deleting a user
from the database. Normally you would use the Setup utility
to remove a user, so most people will not need REMOVE.CMD.
It is, however, useful if your application requires you to
add and remove users dynamically. You can use LoadPRM to
add a user, and REMOVE to remove a user.

:p.To use REMOVE.CMD, you simply execute the command
:xmp.       REMOVE username

:exmp.
where 'username' is the name of the user you want to remove.

:p.:hp2.The ONETIME and LIMITEDUSE utilities:ehp2.

:p.The file ONETIME.CMD is a Rexx script for creating a "use once"
account in the user database. You can also create such an
account using the Setup utility, but the Rexx script is useful
for situations where you want an automated system. For example,
this script can be called from your web server software if you
have a need to do downloads that are controlled by options on
a web page.

:p.A "use once" account is similar to any other user account,
except that the server automatically deletes the account the
first time it is used.

:p.To use ONETIME.CMD, you simply execute the command
:xmp.       ONETIME username password

:exmp.
where the two parameters give the username and password for the
account you are creating.

:p.The script ONETIME.CMD includes a hard-coded permission string
that controls which directories will be accessible to this user.
You should edit this when installing the script. Of course, you
are also free to make any other changes to the script to handle
any special need that your application might have.

:p.LIMITEDUSE.CMD is a variant that allows several logins before
the account is removed.

.***************************************
.*   THE LOGANALYSIS UTILITY
.***************************************

:h1.The LogAnalysis utility
:hp2.The LogAnalysis utility:ehp2.

:p.LogAnalysis.exe is a utility to produce a summary from the
FtpServer user log.  You run it with the command

:xmp.        LogAnalysis logfilename:exmp.

:p.where "logfilename" is the name of the user log file. If no
parameter is supplied, it assumes that the log file name is
FTPUSERS.LOG.

:p.The results are written to standard output. You can redirect
this to a file, for example

:xmp.        LogAnalysis AUGUST.LOG >August.summary
:exmp.

:p.Note that very large log files can cause this program to fail
by running out of memory. If this happens, use a text editor to
split your log file into two or more smaller files.

.***************************************
.*         OTHER UTILITIES
.***************************************

:h1.Other utilities
:hp2.Other utilities:ehp2.

:p.The full set of utilities might change from version to version.
Check the "tools" directory for other scripts.

:p.Note especially the MIGRATE.CMD script, which creates FtpServer
users by interpreting the FTP section of the OS/2 tcp/ip notebook.

:p.A useful utility by Steven Levine to do a variety of operations on
the server can be found at
ftp&colon.//ftp.warpcave.com/.

.***********************************
.*   DEVELOPMENT NOTES
.***********************************

:h1.Development notes

:ul.
:li.:link reftype=hd refid=tools.Development tools:elink.
:li.:link reftype=hd refid=whyM2.Why Modula-2?:elink.
:li.:link reftype=hd refid=bugs.Known bugs:elink.
:li.:link reftype=hd refid=unresolved.Unresolved issues:elink.
:li.:link reftype=hd refid=reporting.Reporting errors:elink.
:li.:link reftype=hd refid=Y2K.Year 2000 compliance:elink.
:eul.

.***********************************
.*   DEVELOPMENT TOOLS
.***********************************

:h2 id=tools.Development tools
:hp2.Development tools:ehp2.

:p.Some people have asked about the compiler I'm using.  (I guess a
lot of people didn't realise that there were Modula-2 compilers
for OS/2.) It's XDS Modula-2, OS/2 native mode version.

You can find out about this, and other Modula-2 compilers for OS/2,
at the web page
.br
     http&colon.//www.pmoylan.org/pages/os2/os2m2.html
.br
Note, however, that that information is seriously out of date.

:p.The XDS compiler is now distributed by Excelsior, whose home page is at
.br
      http&colon.//www.excelsior-usa.com/
.br
There are apparently plans to make the OS/2 version available again,
but it was unavailable at the time this manual was written.
:p.
FtpServer uses some of the modules from the PMOS/2 library.
If you want to know more about PMOS/2, you'll also find that on
my web pages. Source code is available. My web pages are at
http&colon.//www.pmoylan.org/.
:p.
This documentation was prepared with IBM's IPFC help compiler.

.***********************************
.*   WHY MODULA-2?
.***********************************

:h2 id=whyM2.Why Modula-2?
:hp2.Why Modula-2?:ehp2.
:p.
I'm often asked why I chose to code FtpServer in Modula-2. Everyone
else seems to be using C or C++, so why don't I?
:p.
The short answer is that I don't think much of the "everyone else uses it"
argument. If popularity was more important to me than technical merit,
I wouldn't be using OS/2.
:p.
The long answer is contained in a document called "The Case Against C",
which can be found at
ftp&colon.//ftp.pmoylan.org/papers/.
:p.
And the medium-length answer is on this page.
:p.
To begin with, run-time efficiency is not as big an issue as most people
seem to think it is. With modern compiler technology, the main programming
languages (apart from things like BASIC and its derivatives) give about
the same run-time efficiency. C and C++ lose out a little because their
low-level constructs make it hard for the compiler to do a good job at
optimisation; the figures I've seen tend to suggest that a program written in
Modula-2 runs a little faster than the same program written in C or C++.
However, the difference is typically less than 5%, and hardly worth worrying
about.
:p.
So the big issue is development efficiency. For a job like this we can
rule out languages like BASIC and REXX because they're a little too crude;
and we can rule out languages like Fortran because of their poor support
for "systems programming" tasks. We can also rule out a host of lesser-known
languages because of the unavailability of OS/2 compilers. That leaves us
with Pascal, Ada, Oberon, Modula-2, C, and C++.
:p.
I don't use Pascal because Modula-2 is basically an upgraded Pascal, and I
might as well use the improved version.
:p.
I haven't looked into the availability of Ada compilers for OS/2; but in any
case I don't like Ada because of its complexity. The bigger a language is,
the more things there are to go wrong.
:p.
Oberon is a more subjective matter. Some people will tell you that Oberon
is the successor to Modula-2, and is a superior programming language. My
personal opinion is that Oberon has deleted some of the features that make
Modula-2 a good language. I agree, however, that this issue is not entirely
clear-cut.
:p.
That brings us to C and C++. I've done a lot of C and C++ programming over
the years, and it has left me with the feeling that those languages are major
barriers to programming efficiency. It takes me roughly twice the time
to get a C or C++ program working as it does to get a comparable Modula-2
program working. (On some projects I've kept logs to verify this.) The
coding time is roughly the same, but there's a major difference in
debugging time. Everyone I know writes buggy software in C and C++, and
then they take forever trying to track down the bugs. Some developers
give up, and sell the software with the bugs still included.
:p.
There are two main reasons why C software is so bug-prone.
:ol.
:li.Lack of type safety. C is designed in such a way that the compiler can't
do much error checking, so the compiler gives no warnings for things that, in
a type-safe language, would be reported as errors at compile time. You don't
see the errors until execution time, and then you are left wondering what caused
the error.
:li.Poor support for modular programming. You can break up a C program into
modules, but they're not truly independent of one another. A slight change
in one module can have catastrophic effects on other modules. Once a project
grows moderately large, you lose control of your own code.
:eol.
:p.
C++ is a little better in these two respects, but C++ has problems of its own.
The language designers tried to graft high-level features onto a low-level
language, and the result is a mass of inconsistency. A C++ reference manual
is typically several times as thick as manuals for other programming languages,
because every rule has a maze of exceptions and special cases.
:p.
In addition, I've noticed that a lot of C++ programmers seem to have
adopted the philosophy of "let's try this, and hope that it works". The notion
that you shouldn't write code that you don't understand seems to have become
unfashionable. Maybe that's the fault of the language (and its libraries),
maybe not. In any case, it is not the way I prefer to work.
:p.
Ultimately, the reason I use Modula-2 is that it lets me get applications
working quickly, it gives me control of large projects, and it doesn't force
me to spend huge amounts of time on debugging. I'm too old to enjoy the
thrill of tracking down obscure bugs. I like to get something working, and
then be free to move on to other projects.
:p.
Of course, it is difficult to guarantee that any piece of software is bug-free,
no matter what development tools you use. But I can have the next-best
thing, which is an acceptably small error rate.

.***********************************
.*   KNOWN BUGS
.***********************************

:h2 id=bugs.Known bugs
KNOWN BUGS IN SERVER

:ul.

:li.Server shutdown does not work correctly when using
version 4.02k of the TCP/IP stack, although it does work with
earlier versions. Other information I've received seems to
indicate that *ALL* server applications misbehave with
version 4.02k, so it looks as if the fault lies in the
TCP/IP implementation.
I have fixed the problem for version 4.02o, but the fix might
not work with 4.02k - my impression is that versions between
k and o are seriously buggy.

:li.Uploads not accepted if your free disk space is greater than
about 4000 gigabytes. There doesn't seem to be any urgent
need to fix this just yet, but at the rate disk sizes
are growing ...

:eul.

:p.See also :link reftype=hd refid=unresolved.Unresolved issues:elink.

.***********************************
.*   UNRESOLVED ISSUES
.***********************************

:h2 id=unresolved.Unresolved issues
:hp2.Unresolved issues:ehp2.

:p.These are problems that various people have reported, but which
I haven't been able to duplicate. I'd be interested in hearing from
anyone who can confirm either that the problem really exists, or that
it is now fixed.
:p.
:ul.

:li.Reported: the server can be crashed by a client running JavaScript.
This problem is still a mystery to me, as I haven't been able to duplicate
the problem.

:eul.

.***********************************
.*   YEAR 2000 COMPLIANCE
.***********************************

:h2 id=Y2K.Year 2000 compliance
:hp2.Year 2000 compliance:ehp2.

:p.According to my tests, FtpServer should continue running correctly
until 31 December 2079. (It might also continue working after that;
but the OS/2 system clock will not allow me to set the date to
2080 or later.) This assumes that you are using HPFS or JFS volumes.

:p.Systems using the FAT file system will stop working in 2038.
This has nothing to do with FtpServer. It is a fundamental limitation
of FAT.

.***********************************
.*   REPORTING ERRORS
.***********************************

:h2 id=reporting.Reporting errors

:p.If you find any error that's not mentioned
in this document, please report it to peter@pmoylan.org. The
following information would be useful in tracking down the cause of
the error:
:ul.
:li.The version number of the version you are using.
:li.The file errinfo.$$$, if it exists.
:li.Some information about what the server was doing at the time the
problem occurred; for example, the last few lines of the transaction log.
:eul.

:p.The most recent versions of FtpServer include exceptq support, which
you can enable by installing the appropriate DLL. You can find the
exceptq files (version 7.1 or later) at Hobbes and similar OS/2 file repositories.
Once it is installed, a crash will produce a *.TRP file in addition to the
errinfo.$$$ file. In this case I will still need only the errinfo.$$$ file
in the first instance, but if the problem is an obscure one I will probably
ask you for the *.TRP file, which contains more detailed information.

:euserdoc.

