#! /bin/sh
# set -x
# Save the command line for debug outputs
SAVE="$@"
CMD=""
OUTPUT_DIRNAME=""

# Find the correct mc.exe. This could be done by the configure script,
# But as we seldom use the resource compiler, it might as well be done here...
MCC=""
save_ifs=$IFS
IFS=:
for p in $PATH; do 
    if [ -f $p/mc.exe ]; then 
	if [ -n "`$p/mc.exe -? 2>&1 >/dev/null </dev/null \
                 | grep -i \"message compiler\"`" ]; then 
	    MCC=$p/mc.exe
	fi
    fi
done
IFS=$save_ifs

if [ -z "$MCC" ]; then
    echo 'mc.exe not found!' >&2
    exit 1
fi

while test -n "$1" ; do
    x="$1"
    case "$x" in
	-o)
	    shift
	    OUTPUT_DIRNAME="$1";;
	-o/*)
	    y=`echo $x | sed 's,^-[Io]\(/.*\),\1,g'`;
	    OUTPUT_DIRNAME="$y";;
	-I)
	    shift
	    MPATH=`cygpath -m $1`;
	    CMD="$CMD -I\"$MPATH\"";;
	-I/*)
	    y=`echo $x | sed 's,^-[Io]\(/.*\),\1,g'`;
	    MPATH=`cygpath -m $y`;
	    CMD="$CMD -I\"$MPATH\"";;
	*)
	    MPATH=`cygpath -m -a $x`;
	    CMD="$CMD \"$MPATH\"";; 
    esac
    shift
done
p=$$
if [ "X$MC_SH_DEBUG_LOG" != "X" ]; then
    echo rc.sh "$SAVE" >>$MC_SH_DEBUG_LOG
    echo rc.exe $CMD >>$MC_SH_DEBUG_LOG
fi
if [ -n "$OUTPUT_DIRNAME" ]; then
    cd $OUTPUT_DIRNAME
    RES=$?
    if [ "$RES" != "0" ]; then
	echo "mc.sh: Error: could not cd to $OUTPUT_DIRNAME">&2
	exit $RES
    fi
fi
eval $MCC "$CMD"  >/tmp/mc.exe.${p}.1 2>/tmp/mc.exe.${p}.2
RES=$?
tail +2 /tmp/mc.exe.${p}.2 >&2
cat /tmp/mc.exe.${p}.1
rm -f /tmp/mc.exe.${p}.2 /tmp/mc.exe.${p}.1
exit $RES
