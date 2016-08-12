#!/bin/sh
# Project:  COMS S4115, SimpliCty Compiler
# Filename: src/testall.sh
# Authors:  - Rui Gu,           rg2970
#           - Adam Hadar,       anh2130
#           - Zachary Moffitt,  znm2104
#           - Suzanna Schmeelk, ss4648
# Purpose:  * Regression testing script for SimpliCty
#           * Steps through list of files:
#             * Expected to work: compile, run, check output
#             * Expected to fail: compile, check error
# Modified: 2016-07-24

# Path to the LLVM interpreter
LLI="lli"
#LLI="/usr/local/opt/llvm/bin/lli"

# Path to the simpliCty compiler.  Usually "./simplicty.native"
# Try "_build/simplicty.native" if ocamlbuild was unable to create a symbolic link.
SIMPLICTY="../simplicty"

# Set time limit for all operations
ulimit -t 30

globallog=test.log
rm -f $globallog
error=0
globalerror=0
dirout=./test-output/
mkdir test-output 2> /dev/null
keep=0

Usage() {
    echo "Usage: testall.sh [options] [.mc files]"
    echo "-h    Print this help"
    echo "-k    Keep intermediate files"
    echo "-l    Test loop statements"
    echo "-d    Test declaration statements"
    echo "-p    Test print statements"
    echo "-s    Test scan statements"
    echo "-r    Test array statements"
    echo "-t    Test struct statements"
    echo "-f    Test function statements"
    echo "-i    Test if statements"
    echo "-o    Test operator statements"
    echo "-g    Test assignment statements"
    echo "-a    Test all statements"
    exit 1
}

SignalError() {
    if [ $error -eq 0 ] ; then
	echo "FAILED"
	error=1
    fi
    echo "  $1"
}

# Compare <outfile> <reffile> <difffile>
# Compares the outfile with reffile.  Differences, if any, written to difffile
Compare() {
    generatedfiles="$generatedfiles $3"
    echo diff -b ${dirout}$1 $2 ">" $3 1>&2
    diff -b "${dirout}$1" "$2" > "${dirout}$3" 2>&1 || {
	SignalError "$1 differs"
	echo "FAILED $1 differs from $2" 1>&2
    }
}

# Run <args>
# Report the command, run it, and report any errors
Run() {
    echo $* 1>&2
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
    }
}

# RunFail <args>
# Report the command, run it, and expect an error
RunFail() {
    echo $* 1>&2
    eval $* && {
	SignalError "failed: $* did not report an error"
	return 1
    }
    return 0
}

Check() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.sct//'`
    reffile=`echo $1 | sed 's/.sct$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles ${basename}.ll ${basename}.out" &&
    Run "$SIMPLICTY" $1 ">" "${dirout}${basename}.ll" &&
    Run "$LLI" "${dirout}${basename}.ll" ">" "${dirout}${basename}.out" &&
    Compare ${basename}.out ${reffile}.out ${basename}.diff

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
	if [ $keep -eq 0 ] ; then
	    rm -f $generatedfiles
	    rm -f ${dirout}/* 
	fi
	echo "OK"
	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
    fi
}

CheckFail() {
    error=0
    basename=`echo $1 | sed 's/.*\\///
                             s/.sct//'`
    reffile=`echo $1 | sed 's/.sct$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    echo -n "$basename..."

    echo 1>&2
    echo "###### Testing $basename" 1>&2

    generatedfiles=""

    generatedfiles="$generatedfiles ${basename}.err ${basename}.diff" &&
    RunFail "$SIMPLICTY" $1 "2>" "${dirout}${basename}.err" ">>" $globallog &&
    Compare ${basename}.err ${reffile}.err ${basename}.diff

    # Report the status and clean up the generated files

    if [ $error -eq 0 ] ; then
	if [ $keep -eq 0 ] ; then
	    rm -f $generatedfiles
	fi
	echo "OK"
	echo "###### SUCCESS" 1>&2
    else
	echo "###### FAILED" 1>&2
	globalerror=$error
    fi
}

LLIFail() {
  echo "Could not find the LLVM interpreter \"$LLI\"."
  echo "Check your LLVM installation and/or modify the LLI variable in testall.sh"
  exit 1
}

Test() {
    for file in $files
    do
        case $file in
            *test-*)
                Check $file 2>> $globallog
                ;;
            *fail-*)
                CheckFail $file 2>> $globallog
                ;;
            *)
                echo "unknown file type $file"
                globalerror=1
                ;;
        esac
    done

    which "$LLI" >> $globallog || LLIFail

}

TestLoop(){
    echo "Testing Loops"
    files="test/loops/test-*.sct fail/loops/fail-*.sct"
    Test
}

TestDec(){
    echo "Testing Declarations"
    files="test/declaration/test-*.sct fail/declaration/fail-*.sct"
    Test
}

TestPrint(){
    echo "Testing Print"
    files="test/print/test-*.sct fail/print/fail-*.sct"
    Test
}

TestScan(){
    echo "Testing Scan"
    #files="test/scan/test-*.sct fail/scan/fail-*.sct"
    Test
}

TestArray(){
    echo "Testing Array"
    files="test/array/test-*.sct fail/array/fail-*.sct"
    Test
}

TestStruct(){
    echo "Testing Struct"
    #files="test/struct/test-*.sct fail/struct/fail-*.sct"
    Test
}

TestFun(){
    echo "Testing Functions"
    files="test/functions/test-*.sct fail/functions/fail-*.sct"
    Test
}

TestIf(){
    echo "Testing If"
    files="test/if/test-*.sct fail/if/fail-*.sct"
    Test
}

TestOp(){
    echo "Testing Operators"
    #files="test/operators/test-*.sct fail/operators/fail-*.sct"
    files="fail/operators/fail-*.sct"
    Test
}

TestAssign(){
    echo "Testing Assignments"
    files="test/assignment/test-*.sct fail/assignment/fail-*.sct"
    Test
}

TestAll(){
  TestLoop
  TestDec
  TestPrint
  TestScan
  TestArray
  TestStruct
  TestFun
  TestIf
  TestOp
  TestAssign
}

options='khldpsrtfioga*'
while getopts $options optchar; do
    case $optchar in
	k) # Keep intermediate files
	    keep=1
	    ;;
	h) # Help
	    Usage; exit 0;
	    ;;
        l) # Test loop statements
	    TestLoop; exit 0;
            ;;
        d) # Test declaration statements
	    TestDec; exit 0;
            ;;
        p) # Test print statements
	    TestPrint; exit 0;
            ;;
        s) # Test scan statements
	    TestScan; exit 0;
            ;;
        r) # Test array statements
            TestArray; exit 0;
            ;;
        t) # Test struct statements
            TestStruct; exit 0;
            ;;
        f) # Test fun statements
            TestFun; exit 0;
            ;;
        i) # Test if statements
            TestIf; exit 0;
            ;;
        o) # Test operator statements
            TestOp; exit 0;
            ;;
        g) # Test assign statements
            TestAssign; exit 0;
            ;;
        a) # Test all statements
            TestAll; exit 0;
            ;;
        *) # Default
            TestAll; exit 0;
            ;;
    esac
done
TestAll
shift `expr $OPTIND - 1`

exit $globalerror
