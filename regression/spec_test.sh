#/bin/bash

for i in *.gtp.in ; do 
	../interface/gnugo <$i >${i/\.in/.out2}
	if [ `diff ${i/\.in/.out} ${i/\.in/.out2} | wc -l` -gt 0 ]
	then
		echo "Difference for $i";
	fi
done

