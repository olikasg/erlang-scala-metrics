#! /bin/sh
# set -x
# Save the command line for debug outputs
SAVE="$@"
kernel_libs="kernel32.lib advapi32.lib"
gdi_libs="gdi32.lib user32.lib comctl32.lib comdlg32.lib shell32.lib"
DEFAULT_LIBRARIES="$kernel_libs $gdi_libs"

CMD=""
STDLIB=MSVCRT.LIB
DEBUG_BUILD=false
STDLIB_FORCED=false
BUILD_DLL=false
OUTPUT_FILENAME=""

while test -n "$1" ; do
    x="$1"
    case "$x" in
	-dll| -DLL)
	    BUILD_DLL=true;; 
	-L/*|-L.*)
	    y=`echo $x | sed 's,^-L\(.*\),\1,g'`;
	    MPATH=`cygpath -m $y`;
	    CMD="$CMD -libpath:\"$MPATH\"";; 
	-lMSVCRT|-lmsvcrt)
	    STDLIB_FORCED=true;
	    STDLIB=MSVCRT.LIB;; 
	-lMSVCRTD|-lmsvcrtd)
	    STDLIB_FORCED=true;
	    STDLIB=MSVCRTD.LIB;; 
	-lLIBCMT|-llibcmt)
	    STDLIB_FORCED=true;
	    STDLIB=LIBCMT.LIB;; 
	-lLIBCMTD|-llibcmtd)
	    STDLIB_FORCED=true;
	    STDLIB=LIBCMTD.LIB;; 
	-lsocket)
	    DEFAULT_LIBRARIES="$DEFAULT_LIBRARIES WS2_32.LIB";;
	-l*)
	    y=`echo $x | sed 's,^-l\(.*\),\1,g'`;
	    MPATH=`cygpath -m $y`;
	    CMD="$CMD \"${MPATH}.lib\"";; 
	-g)
	    DEBUG_BUILD=true;;
	-pdb:none|-incremental:no)
	    ;;
	-implib:*)
	    y=`echo $x | sed 's,^-implib:\(.*\),\1,g'`;
	    MPATH=`cygpath -m $y`;
	    CMD="$CMD -implib:\"${MPATH}\"";; 
	-def:*)
	    y=`echo $x | sed 's,^-def:\(.*\),\1,g'`;
	    MPATH=`cygpath -m $y`;
	    CMD="$CMD -def:\"${MPATH}\"";; 
	-o)
	    shift
	    MPATH=`cygpath -m $1`;
	    OUTPUT_FILENAME="$MPATH";;
	-o/*)
	    y=`echo $x | sed 's,^-[Io]\(/.*\),\1,g'`;
	    MPATH=`cygpath -m $y`;
	    OUTPUT_FILENAME="$MPATH";;
	/*)
	    MPATH=`cygpath -m $x`;
	    CMD="$CMD \"$MPATH\"";; 
	*)
	    y=`echo $x | sed 's,",\\\",g'`;
	    CMD="$CMD \"$y\"";;
    esac
    shift
done
if [ $DEBUG_BUILD = true ]; then
    linktype="-debug -pdb:none"
    if [ $STDLIB_FORCED = false ]; then
	STDLIB=MSVCRTD.LIB
    fi
fi
# Generate a PDB 
linkadd_pdb=""
case "$OUTPUT_FILENAME" in
    *.exe|*.EXE)
	    fn=`echo "$OUTPUT_FILENAME" | sed 's,[eE][xX][eE]$,,g'`;
	    linkadd_pdb="-pdb:\"${fn}pdb\"";;
    *.dll|*.DLL)
	    fn=`echo "$OUTPUT_FILENAME" | sed 's,[dD][lL][lL]$,,g'`;
	    linkadd_pdb="-pdb:\"${fn}pdb\"";;
    "")
	    linkadd_pdb="-pdb:\"a.pdb\"";;
    *)
	    linkadd_pdb="-pdb:\"${OUTPUT_FILENAME}.pdb\"";;
esac
	
    linktype="-debug $linkadd_pdb"

if [ $BUILD_DLL = true ];then
    case "$OUTPUT_FILENAME" in
	*.exe|*.EXE)
	    echo "Warning, output set to .exe when building DLL" >&2
	    CMD="-dll -out:\"$OUTPUT_FILENAME\" $CMD";
	    OUTPUTRES="${OUTPUT_FILENAME}\;2";
	    MANIFEST="${OUTPUT_FILENAME}.manifest";;
	*.dll|*.DLL)
	    CMD="-dll -out:\"$OUTPUT_FILENAME\" $CMD";
	    OUTPUTRES="${OUTPUT_FILENAME}\;2";
	    MANIFEST="${OUTPUT_FILENAME}.manifest";;
	"")
	    CMD="-dll -out:\"a.dll\" $CMD";
	    OUTPUTRES="a.dll\;2";
	    MANIFEST="a.dll.manifest";;
	*)
	    CMD="-dll -out:\"${OUTPUT_FILENAME}.dll\" $CMD";
	    OUTPUTRES="${OUTPUT_FILENAME}.dll\;2";
	    MANIFEST="${OUTPUT_FILENAME}.dll.manifest";;
    esac
else
    case "$OUTPUT_FILENAME" in
	*.exe|*.EXE)
	    CMD="-out:\"$OUTPUT_FILENAME\" $CMD";
	    OUTPUTRES="${OUTPUT_FILENAME}\;1"
	    MANIFEST="${OUTPUT_FILENAME}.manifest";;
	*.dll|*.DLL)
	    echo "Warning, output set to .dll when building EXE" >&2
	    CMD="-out:\"$OUTPUT_FILENAME\" $CMD";
	    OUTPUTRES="${OUTPUT_FILENAME}\;1";
	    MANIFEST="${OUTPUT_FILENAME}.manifest";;
	"")
	    CMD="-out:\"a.exe\" $CMD";
	    OUTPUTRES="a.exe\;1";
	    MANIFEST="a.exe.manifest";;
	*)
	    CMD="-out:\"${OUTPUT_FILENAME}.exe\" $CMD";
	    OUTPUTRES="${OUTPUT_FILENAME}.exe\;1";
	    MANIFEST="${OUTPUT_FILENAME}.exe.manifest";;
    esac
fi    
	    
p=$$
CMD="$linktype -nologo -incremental:no $CMD $STDLIB $DEFAULT_LIBRARIES"
if [ "X$LD_SH_DEBUG_LOG" != "X" ]; then
    echo ld.sh "$SAVE" >>$LD_SH_DEBUG_LOG
    echo link.exe $CMD >>$LD_SH_DEBUG_LOG
fi
eval link.exe "$CMD"  >/tmp/link.exe.${p}.1 2>/tmp/link.exe.${p}.2
RES=$?
CMANIFEST=`cygpath $MANIFEST`
if [ "$RES" = "0" -a -f "$CMANIFEST" ]; then
    eval mt.exe -nologo -manifest "$MANIFEST" -outputresource:"$OUTPUTRES" >>/tmp/link.exe.${p}.1 2>>/tmp/link.exe.${p}.2
    RES=$?
    if [ "$RES" != "0" ]; then
	REMOVE=`echo "$OUTPUTRES" | sed 's,\\\;[12]$,,g'`
	CREMOVE=`cygpath $REMOVE`
	rm -f "$CREMOVE"
    fi
    rm -f "$CMANIFEST"
fi
    
tail +2 /tmp/link.exe.${p}.2 >&2
cat /tmp/link.exe.${p}.1
rm -f /tmp/link.exe.${p}.2 /tmp/link.exe.${p}.1
exit $RES
