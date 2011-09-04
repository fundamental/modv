#!/bin/sh
echo testing
$1 $2 $3
diff -wB $3 $4
