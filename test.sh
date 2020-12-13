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

assert 0 '{ return 0; }'
assert 42 '{ return 42; }'

assert 25 '{ return 2+23; }'
assert 11 '{ return 23-12; }'
assert 57 '{ return 42+3+12; }'
assert 33 '{ return 42+3-12; }'
assert 33 '{ return 42  + 3- 12; }'

assert 35 '{ return 5*7; }'
assert 38 '{ return 3+5*7; }'
assert 4 '{ return 8/2; }'
assert 7 '{ return 3+8/2; }'
assert 56 '{ return (3+5)*7; }'
assert 5 '{ return (3+8)/2; }'

assert 10 '{ return -10+20; }'
assert 10 '{ return --10; }'
assert 10 '{ return -- +10; }'
assert 10 '{ return 20+(-10); }'

assert 1 '{ return 3==3; }'
assert 0 '{ return 3==4; }'
assert 0 '{ return 3!=3; }'
assert 1 '{ return 3!=4; }'

assert 1 '{ return 24<42; }'
assert 0 '{ return 42<24; }'
assert 0 '{ return 24<24; }'
assert 0 '{ return 24>42; }'
assert 1 '{ return 42>24; }'
assert 0 '{ return 24>24; }'
assert 1 '{ return 24<=42; }'
assert 0 '{ return 42<=24; }'
assert 1 '{ return 24<=24; }'
assert 0 '{ return 24>=42; }'
assert 1 '{ return 42>=24; }'
assert 1 '{ return 24>=24; }'

assert 3 '{ 1; 2; return 3; }'

assert 3 '{ a=3; return a; }'
assert 5 '{ a=3; z=5; return z; }'
assert 8 '{ a=3; z=5; return a+z; }'
assert 6 '{ a=b=3; return a+b; }'

assert 3 '{ foo=3; return foo; }'
assert 9 '{ foo=3; bar=6; return foo+bar; }'
assert 12 '{ foo=bar=6; return foo+bar; }'
assert 6 '{ foo123=6; return foo123; }'

assert 1 '{ return 1; return 2; 3; }'
assert 2 '{ 1; return 2; 3; }'
assert 3 '{ 1; 2; return 3; }'
assert 2 '{ 1; return 2; return 3; }'

assert 3 '{ { 1; {2;} return 3; } }'

echo OK
