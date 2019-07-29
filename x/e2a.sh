filelist=`ls | grep ".edf" | sed s/.edf//`
for f in $filelist 
do
	echo $f
	
	./edf2asc -s -miss -1.0 $f.edf
	cat $f.asc | awk 'BEGIN{FS=" "}{print $1"\t"$2"\t"$3"\t"$4}' > dat.tmp
	mv dat.tmp ../asc/$f.dat
	rm $f.asc
	
	./edf2asc -e $f.edf
	cat $f.asc | grep -E 'MSG|START' > $f.msg
	mv $f.msg ../asc/
	rm $f.asc
	
	mv $f.edf ../edf/
done
