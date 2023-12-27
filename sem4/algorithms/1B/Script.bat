@ECHO OFF 
SET /A "index = 1"
SET /A "count = 1000"
:while
if %index% leq %count% (
   echo The value of index is %index%
   SET /A "index = index + 1"
   python gen1B.py
python 1B.py
python 1Bdumb.py

fc /b dumb.txt smart.txt > nul
if errorlevel 1 (
	echo different
	type dumb.txt
	echo "-----"
	type smart.txt
	echo "-----"
	type test.txt
) else (
	echo same
)
   goto :while
)

	Pause