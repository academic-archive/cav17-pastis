#!/bin/sh

ifile=$1

case "$ifile" in
	*.c)
		cfile=$ifile
		ofile=/tmp/tmp.bc
		;;
	*.o|*.bc)
		cfile=
		ofile=$ifile
		;;
	*)
		echo "usage: ctopng file.{c,o,bc}" >&2
		exit 1
		;;
esac

( test -z "$cfile" ||
  clang -emit-llvm -c $cfile -o /tmp/tmp.bc
  ) &&
./llvm-reader -dot $ofile > /dev/null &&
mv $ofile.dot /tmp/tmp.dot &&
dot -Tpng /tmp/tmp.dot > /tmp/tmp.png &&
feh /tmp/tmp.png
