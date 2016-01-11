#!/usr/bin/perl

use Cwd;

sub esc_chars {
    my $s = shift;
    $s =~ s/([ ;<>\*\|`&\$!#\(\)\[\]\{\}:'"\\])/\\$1/g;
    return $s;
}

$CC="gcc";

$LOGFILE=$ENV{LOGFILE};

if ($LOGFILE ne '') {
    open FH, ">>$LOGFILE";
    $cwd = getcwd();
    print FH join(' ', 'cd', esc_chars($cwd)); print FH "\n";
    @args = map(esc_chars($_), @ARGV);
    print FH join(' ', 'duppy', @args); print FH "\n";
    close FH;
}
exec $CC, @ARGV;
