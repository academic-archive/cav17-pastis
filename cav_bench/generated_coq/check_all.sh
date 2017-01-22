tmout="5m"

check() {
	pat=$1
	out=$2
	cat /dev/null > $out
	for vfile in `ls *.v | awk "$pat"`
	do
		printf "$vfile " >> $out
		timeout $tmout coqc -Q coq pasta $vfile
		case $? in
		124)
			echo "TIMEOUT" >> $out
			;;
		*)
			if test -f ${vfile}o
			then
				echo "CHECKED" >> $out
			else
				echo "FAILED" >> $out
			fi
			;;
		esac
	done
}

check "NR%4==0" ../coqout.0 &
check "NR%4==1" ../coqout.1 &
check "NR%4==2" ../coqout.2 &
check "NR%4==3" ../coqout.3 &

wait %1 %2 %3 %4
