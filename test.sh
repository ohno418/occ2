./occ > tmp.s
gcc -static -o tmp tmp.s
./tmp

echo $?
