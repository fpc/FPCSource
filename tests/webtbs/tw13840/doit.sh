if [ $# != 1 ]; then
  echo Usage: $0 ppc_to_test
  exit 1
fi

rm *.ppu
$1 tw13840d
touch tw13840a.pp
$1 tw13840b -dchanged
$1 tw13840d -dchanged
./tw13840d
