for filename in ./test/lamb/*.lamb; do
	echo " "
	echo "$filename"
	./interpretor "$filename" $*
	if [ $? != 0 ]
		then exit 1
	fi
done
