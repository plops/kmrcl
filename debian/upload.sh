#!/bin/bash -e

dup kmrcl -Ufiles.b9.com -D/home/ftp/kmrcl  -C"(umask 022; cd /srv/www/html/kmrcl; make install)" -su $*



