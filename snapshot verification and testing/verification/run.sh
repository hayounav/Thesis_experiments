#!/usr/bin/env bash

trap myexit EXIT

list_descendants ()
{
  local children=$(ps -o pid= --ppid "$1")

  for pid in $children
  do
    list_descendants "$pid"
  done

  echo "$children"
}

myexit(){
    kill $(list_descendants $$) 2>/dev/null
}

usage() {
cat <<EOF
Usage: $0 -n <algorithm_name> [optional arguments]

Optional arguments include:
       -t <bug|verification>	Set the type of the experiment to run. 
       	  			Defualt value: bug
       -p <#of processess>	Set the number of processes for the experiment.
       	       			Default value: 3
       -b <depth>		Set a bound on the number of operations each process may execute.
       	  			If not specified, no bound is enforced.
       -s 		       	Configure the experiment to explore only simple histories.
       				If not specified, full histories are explored.
       -a			Configure the experiment to use the automata-based method. Implies -s.
       				If not specified, PATs normal linearizability method is used.

Example:
	To run the bug-finding experiment on the bowman algorithm for 4 processes 
	with a bound of 20 operations per process exploring only simple histories, 
	run the following command: $0 -n bowman -p 4 -b 3 -s
EOF
}

PAT_EXECUTABLE="./Process Analysis Toolkit 3.5.1/PAT3.Console.exe"
DIR_RELATIVE_TO_PAT=".."
SPEC_AND_INTERFACE="Specs/"
WORKSPACE="Workspace"

TEST_TYPE="bug"
NUM_PROCS=3

while getopts "n:t:p:b:sah" arg; do
    case $arg in
	t) TEST_TYPE=$OPTARG;;
	n) TEST_NAME=$OPTARG;;
	p) NUM_PROCS=$OPTARG;;
	b) BOUNDED=$OPTARG;;
	s) SIMPLE="simple";;
	a) AUTOMATA="automata"
	   SIMPLE="simple";;
	\? | h | "help") usage; exit 1;;
    esac
done

if ((OPTIND == 1))
then
    echo "No options specified. Must at least specify an algorithm to run."
    usage
    exit 1
fi

if [ "$TEST_TYPE" != "bug" -a "$TEST_TYPE" != "verification" ]; then
    echo "Invalid experiment type given: $TEST_TYPE"
    usage
    exit 1
fi

if [ $TEST_TYPE = "bug" ]; then
    TEST_DIR="BugFinding"
    RUNTIME=600
else
    TEST_DIR="Verification"
    RUNTIME=7200
fi

TEST_FILENAME=$TEST_NAME

case $TEST_NAME in
    jayanti_alg4)
	VIEW_VAR="mySS[pid]";;
    riany)
	VIEW_VAR="result[i]";;
    bowman)
	VIEW_VAR="B[i]";;
    *)
	VIEW_VAR="V[pid]";;
esac

if [ "$SIMPLE" ]; then
    SPEC_AND_INTERFACE="${SPEC_AND_INTERFACE}simple_"
    TEST_FILENAME="${TEST_FILENAME}_simple"
    if [ "$AUTOMATA" ]; then
	SPEC_AND_INTERFACE="${SPEC_AND_INTERFACE}automata_"
    fi
else
    SPEC_AND_INTERFACE="${SPEC_AND_INTERFACE}normal_"
fi

if [ "$BOUNDED" ]; then
    SPEC_AND_INTERFACE="${SPEC_AND_INTERFACE}bounded.csp"
else
    SPEC_AND_INTERFACE="${SPEC_AND_INTERFACE}unbounded.csp"
fi

mkdir -p $WORKSPACE
TEST_FILE="${TEST_DIR}/${TEST_FILENAME}.csp"
TMP_TEST_FILE="$WORKSPACE/test"
TMP_TEST_OUTPUT="$WORKSPACE/output" #needs to be relative to PAT_EXECUTABLE

I=0
SCAN_RESPONSE=""
while [ $I -lt $NUM_PROCS ]; do
    SCAN_RESPONSE="${SCAN_RESPONSE}.${VIEW_VAR}[$I]"
    ((I++))
done

I=0
ABS_SCAN="ScanAbs(pid) = startScan.pid -> FinishAbs(pid"
while [ $I -lt $NUM_PROCS ]; do
    ABS_SCAN="${ABS_SCAN}, R[$I]"
    ((I++))
done
ABS_SCAN="${ABS_SCAN});\n"
I=0
ABS_SCAN="${ABS_SCAN}FinishAbs(pid"
while [ $I -lt $NUM_PROCS ]; do
    ABS_SCAN="${ABS_SCAN}, r$I"
    ((I++))
done

I=0
ABS_SCAN="${ABS_SCAN}) = tau -> endScan.pid"
while [ $I -lt $NUM_PROCS ]; do
    ABS_SCAN="${ABS_SCAN}.r$I"
    ((I++))
done
ABS_SCAN="${ABS_SCAN} -> Skip;"

cp $TEST_FILE $TMP_TEST_FILE
cat $SPEC_AND_INTERFACE >> $TMP_TEST_FILE
sed -i "s/<NUM-PROCS>/${NUM_PROCS}/; s/<SCAN-RESPONSE>/${SCAN_RESPONSE}/; s/<ABS-SCAN>/${ABS_SCAN}/; s/<LIMIT-RUN>/${BOUNDED}/" $TMP_TEST_FILE

function Progress () {
    local spinner=("-" "\\" "|" "/")
    local i=0
    while true; do
	printf " %s\r" ${spinner[$i]}
	((i=(i+1)%4))
	sleep 0.5
    done; echo
}

echo "Experiment configuration (max runtime is $RUNTIME seconds):"
echo -e "\tName,Type,Simple,Automata,Bound,#Procs"
echo -e "\t$TEST_NAME,$TEST_TYPE,$SIMPLE,$AUTOMATA,$BOUNDED,$NUM_PROCS"
echo
Progress &
TIMER_PID=$!

if [ "$AUTOMATA" ]; then
    AUTOS=(1 2 I J AP)
    AUTO_PIDS=()
    for A in "${AUTOS[@]}"; do
	sed "s/<ASSERT-AUTOMATON>/#assert Verify${A}() reaches goal;/" $TMP_TEST_FILE > ${TMP_TEST_FILE}_$A
	timeout $RUNTIME "$PAT_EXECUTABLE" -nc -csp ${TMP_TEST_FILE}_$A ${DIR_RELATIVE_TO_PAT}/${TMP_TEST_OUTPUT}_${A} && echo ${TMP_TEST_OUTPUT}_$A > who_won &
	AUTO_PIDS+=($!)
    done

    if [ "$TEST_TYPE" = "bug" ]; then
	wait -n
	while [ -f who_won ] && [ -n "$(cat `cat who_won` | grep 'NOT valid')" -a -n "$(jobs -p)" ]; do
	    wait -n
	done
	
	EXTRA_MEM=`tasklist.exe /fi "IMAGENAME eq PAT3.Console.exe" | tail -n 5 | awk '{gsub(/,/,"",$5); sum += $5} END {print sum}'`

	kill $TIMER_PID 2>/dev/null
	if [ -n "$(jobs -p)" ]; then kill $(list_descendants $$) 2>/dev/null; fi
	if [ -f who_won ]; then
	    TMP_TEST_OUTPUT=$(cat who_won)
	    rm who_won
	fi
    else
	wait "${AUTO_PIDS[@]}"
	kill $TIMER_PID 2>/dev/null
	for A in "${AUTOS[@]}"; do
	    if [ -z "$(grep 'NOT valid' ${TMP_TEST_OUTPUT}_${A})" ]; then
		STATS=",,,"
		break
	    fi
	    STATS+=$(awk -F: '/Visited States/{sts = $2} /Total Transitions/{trans = $2} /Time Used/{time = $2} /Memory Used/{mem = $2} END{ if (sts != 0) { printf "%d,%d,%f,%f",sts,trans,time,mem} else {print "0,0,0,0"}}' ${TMP_TEST_OUTPUT}_${A})"\n"
	done
    fi
else
    timeout $RUNTIME "$PAT_EXECUTABLE" -nc -csp $TMP_TEST_FILE ${DIR_RELATIVE_TO_PAT}/$TMP_TEST_OUTPUT &
    wait -n
    kill $TIMER_PID 2>/dev/null
fi

if [ "$STATS" ]; then
    FOO=$(echo -e ${STATS[@]} | awk -F, -v max_t=0 '{states+=$1; trans+=$2; max_t=($3>max_t?$3:max_t); mem+=$4} END {printf "%d,%d,%f,%f",states,trans,max_t,mem}')
    STATS=$FOO
else
    if [ -f "$TMP_TEST_OUTPUT" ]; then
	STATS=$(awk -F: '/Visited States/{sts = $2} /Total Transitions/{trans = $2} /Time Used/{time = $2} /Memory Used/{mem = $2} END{ if (sts != 0) { printf "%d,%d,%f,%f",sts,trans,time,mem} else {print "0,0,0,0"}}' $TMP_TEST_OUTPUT)
    fi
fi

RESULTS_FILE=results.csv
if ! [ -f $RESULTS_FILE ]; then
    echo "Name,Type,Simple,Automata,Bound,#Procs,#States,#Transitions,Time,Memory,Extra Memory" > $RESULTS_FILE
fi

echo $TEST_NAME,$TEST_TYPE,$SIMPLE,$AUTOMATA,$BOUNDED,$NUM_PROCS,$STATS,"${EXTRA_MEM}" >> $RESULTS_FILE

echo Results:
echo -e "\t#States,#Transitions,Time,Memory,Extra Memory"
echo -e "\t$STATS,$EXTRA_MEM"
echo -e "(These results have been recorded in $RESULTS_FILE)"

rm -rf $WORKSPACE
