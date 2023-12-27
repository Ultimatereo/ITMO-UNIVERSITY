@ECHO OFF
SET /A "index = 1"
SET /A "count = 1000"
:while
if %index% leq %count% (
   echo The value of index is %index%
   SET /A "index = index + 1"
   python gen2C.py
python 2C.py

if errorlevel 1 (
	echo different
	type smart.txt
	echo "-----"
	type test.txt
) else (
	echo same
)
   goto :while
)

	Pause