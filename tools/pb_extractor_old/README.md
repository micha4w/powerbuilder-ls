## Creating lib files for PB
```ps1
$lib="pbshr"
echo 'LIBRARY $lib.dll' > $lib.def
echo 'EXPORTS' >> $lib.def
cat .\$lib.exports.fn | awk '{ print $4 " @" $1 }' >> $lib.def
lib /MACHINE:x86 /def:$lib.def /out:$lib.lib 
```