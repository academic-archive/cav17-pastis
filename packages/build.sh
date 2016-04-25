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

if ! test -d sandbox
then
	mkdir sandbox
	mkdir sandbox/bin
	mkdir sandbox/lib
	mkdir sandbox/lib/ocaml
	mkdir sandbox/lib/ocaml/caml
fi

export SANDBOX=`pwd`/sandbox

echo "** Building camlidl-1.05"
pack=camlidl-1.05
cd $pack
make > build.out 2> build.err             || fail build
make install > install.out 2> install.err || fail install
cd ..
echo "    done"
