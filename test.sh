#!/bin/bash

readonly BIN='./208dowels'
readonly TMP='/tmp/208dowels_test'

(make > /dev/null) || exit 1


readonly TESTS_PARAMS1="6 4 10 18 20 19 11 5 7"
readonly TESTS_PARAMS2="6 4 10 8 20 19 11 5 17"
readonly TESTS_PARAMS3="4 5 13 19 20 16 12 7 4"

readonly EXPECTED_RES1="examples/example1.txt"
readonly EXPECTED_RES2="examples/example2.txt"
readonly EXPECTED_RES3="examples/example3.txt"

$BIN $TESTS_PARAMS1 > $TMP
diff $TMP $EXPECTED_RES1
echo

$BIN $TESTS_PARAMS2 > $TMP
diff $TMP $EXPECTED_RES2
echo

$BIN $TESTS_PARAMS3 > $TMP
diff $TMP $EXPECTED_RES3

rm $TMP
