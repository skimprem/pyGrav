 #!/usr/bin/perl
 use POSIX;
 unlink "os.txt";
 my ($sysname, $nodename, $release, $version, $machine ) = POSIX::uname();
 if ($sysname =~ m/cygwin/i) {$sysname = "LINUX"}
 elsif ($sysname =~ m/linux/i) {$sysname = "LINUX"}
 elsif ($sysname =~ m/windows/i) {$sysname = "WINNT"}
 open (FIC,">os.txt");
 print FIC "$sysname\n";
 close(FIC);
