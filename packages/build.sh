export GMP_PREFIX=/usr
export MPFR_PREFIX=/usr
export CAML_PREFIX=`ocamlc -where`/../..

pack_camlidl=camlidl-1.05
pack_apron=apron-0.9.10
pack_clp=Clp-1.15.7

fail() {
	echo "failure, details in"
	echo "    `pwd`/$1.out"
	echo "    `pwd`/$1.err"
	exit 1
}

if ! test -f `pwd`/build.sh
then
	echo "run build.sh in its directory" >&2
	exit 1
fi

if test "$1" = clean
then
	rm -fr sandbox
	for pack in $pack_camlidl $pack_apron $pack_clp
	do
		echo "** Cleaning $pack"
		( cd $pack; make clean; ) >/dev/null 2>&1
	done
	exit 0
fi

if ! test -d sandbox
then
	mkdir sandbox
	mkdir sandbox/bin
	mkdir sandbox/lib
	mkdir sandbox/lib/ocaml
	mkdir sandbox/lib/ocaml/caml
fi

export SANDBOX=`pwd`/sandbox

pack=$pack_camlidl
echo "** Building $pack"
cd $pack
make > build.out 2> build.err             || fail build
make install > install.out 2> install.err || fail install
cd ..
echo "    done"

pack=$pack_apron
echo "** Building $pack"
cd $pack
make > build.out 2> build.err             || fail build
make install > install.out 2> install.err || fail install
cd ..
echo "    done"

pack=$pack_clp
echo "** Building $pack"
cd $pack
./configure --enable-static --prefix=$SANDBOX > configure.out 2> configure.err || fail configure
make > build.out 2> build.err             || fail build
make install > install.out 2> install.err || fail install
cd ..
echo "    done"

touch $SANDBOX/done
