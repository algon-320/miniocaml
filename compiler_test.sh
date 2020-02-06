#!/usr/bin/env bash

EXECUTABLE="a.out"
MINIOCAML="./miniocaml"

GREEN=$(tput setaf 2)
RED=$(tput setaf 1)
RESET=$(tput sgr0)

passed=0
failed=0

function test() {
  local filename=$1
  local expect="$2"
  local actual="(undefined)"
  if $MINIOCAML $filename -o $EXECUTABLE && actual="$(./${EXECUTABLE})" && [ "$actual" = "$expect" ]; then
    echo "$filename - ${GREEN}ok${RESET}";
    passed=$((passed + 1))
  else
    echo "$filename - ${RED}fail${RESET}";
    echo "expected:"
    echo "\"${expect}\""
    echo "actual:"
    echo "\"${actual}\""
    failed=$((failed + 1))
  fi
}

function test_stdin() {
  local filename=$1
  local input="$2"
  local expect="$3"
  local actual="(undefined)"
  if $MINIOCAML $filename -o $EXECUTABLE && actual="$(echo $input | ./${EXECUTABLE})" && [ "$actual" = "$expect" ]; then
    echo "$filename - ${GREEN}ok${RESET}";
    passed=$((passed + 1))
  else
    echo "$filename - ${RED}fail${RESET}";
    echo "expected:"
    echo "\"${expect}\""
    echo "actual:"
    echo "\"${actual}\""
    failed=$((failed + 1))
  fi
}

test examples/fun1.mml 30
test examples/fun2.mml 30
test examples/fun3.mml 12 
test examples/fun4.mml 3 
test examples/fun5.mml 12 
test examples/head1.mml 1
test examples/head2.mml "[1; 2; 3; ]"
test examples/let1.mml "[4; 1; 2; 3; ]"
test examples/list1.mml 4
test examples/list2.mml "[123; ]"
test examples/list3.mml "[[1; 2; 3; ]; [4; 5; ]; ]"
test examples/shadow1.mml 456
test examples/unit1.mml "()"
test examples/unit2.mml "[(); (); (); ]"
test examples/unit3.mml "()"
test examples/list_of_fun1.mml "246
15129"
test examples/rec1.mml "11"
test examples/rec2.mml "10"
test examples/rec3.mml "55"
test examples/rec4.mml "[5; 4; 3; 2; 1; ]"
test examples/fib1.mml "832040"
test examples/match1.mml "2"
test examples/match2.mml "1"
test examples/match3.mml "999"

test_stdin examples/read_int1.mml "10" "55"
test_stdin examples/read_int2.mml "3 1 2 3" "[1; 2; 3; ]"

rm $EXECUTABLE

echo "----------------"
echo "passed: ${GREEN}${passed}${RESET}"
echo "failed: ${RED}${failed}${RESET}"
