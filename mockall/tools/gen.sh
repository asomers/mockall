#! /bin/sh
# Preprocess the given test and compare its output to the saved output in pp_old

TOOLSDIR=`dirname ${0}`
PP_OLD=${TOOLSDIR}/../pp_old
PP=${TOOLSDIR}/../pp

t=$1
if [ -z "$t" ]; then
	echo "Usage: gen.sh <testcase>" >&2
	exit 2
fi

env MOCKALL_DEBUG=1 cargo check --all-features --test $t > ${PP}/$t.rs; rustfmt ${PP}/$t.rs
echo "diff stat: " `diff ${PP_OLD}/$t.rs ${PP}/$t.rs | wc`
