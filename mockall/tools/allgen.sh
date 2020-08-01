#! /bin/sh
# Preprocess every integration test and output the generated code to a new file
# in the directory $1, or mockall/pp_old by default.

TOOLSDIR=`dirname ${0}`
DEFAULT_ODIR=${TOOLSDIR}/../pp_old
ODIR=${PWD}/${1:-$DEFAULT_ODIR}
mkdir -p ${ODIR}

cd ${TOOLSDIR}/../tests
#echo ODIR is $ODIR
#exit 0
for t in `ls *.rs | sed 's/\.rs//'`; do
	env MOCKALL_DEBUG=1 cargo +nightly check --all-features --test $t > ${ODIR}/$t.rs || break;
	rustfmt ${ODIR}/$t.rs ;
done
