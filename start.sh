#!/bin/sh
cd `dirname $0`
exec erl +K +P 134217727 -pa $PWD/ebin $PWD/deps/*/ebin -boot hm_rel-1

