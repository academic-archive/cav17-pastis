tmout="3s"

opt="-degree 2"
bin=`mktemp`
cp ../../source/Driver.native $bin
chmod +x $bin

for test in Inputs/Loopus/*
do
	echo "--- File $test"
	timeout $tmout $bin $opt $test
	code=$?
	case $code in
	124)
		echo "[TIMEOUT]"
		;;
	0)
                mv generated_coq.v ../generated_coq/`basename $test | sed 's,\.,,g'`.v
		echo "[OK]"
		;;
	*)
		echo "[FAILURE $code]"
		;;
	esac
done

rm -f $bin
