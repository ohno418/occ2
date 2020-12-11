#!/bin/bash
assert() {
  expected="$1"
  input="$2"

  ./occ "$input" > tmp.s
  gcc -static -o tmp tmp.s
  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert 0 0
assert 42 42
assert 25 '2+23'
assert 11 '23-12'
assert 57 '42+3+12'
assert 33 '42+3-12'
assert 33 '42  + 3- 12'
assert 35 '5*7'
assert 38 '3+5*7'
assert 4 '8/2'
assert 7 '3+8/2'
assert 56 '(3+5)*7'
assert 5 '(3+8)/2'
assert 10 '-10+20'
assert 10 '--10'
assert 10 '-- +10'
assert 10 '20+(-10)'
assert 1 '3==3'
assert 0 '3==4'
assert 0 '3!=3'
assert 1 '3!=4'
assert 1 '24<42'
assert 0 '42<24'
assert 0 '24<24'
assert 0 '24>42'
assert 1 '42>24'
assert 0 '24>24'

echo OK
