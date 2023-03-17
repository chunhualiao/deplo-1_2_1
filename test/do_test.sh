#!/bin/sh

echo '+++  TEST FUNCTIONALITY DISABLED     +++'
echo '+++  (it could give false negatives) +++'
exit 1


cd `dirname $0`
deplo=../obj/deplo

$deplo inputs > tmp/x1.dot
if diff tmp/x1.dot outputs/single-parameter.dot ; then
    echo SUCCESS;
else
    echo FAILURE
fi

$deplo input=inputs > tmp/x2.dot
if diff tmp/x2.dot outputs/single-parameter.dot ; then
    echo SUCCESS;
else
    echo FAILURE
fi

$deplo input=inputs ignore=@ignore.txt > tmp/x3.dot
if diff tmp/x3.dot outputs/with-ignore.dot ; then
    echo SUCCESS;
else
    echo FAILURE
fi

$deplo input=inputs ignore=@ignore.txt trim=@trim.txt > tmp/x4.dot
if diff tmp/x4.dot outputs/with-trim-ignore.dot ; then
    echo SUCCESS;
else
    echo FAILURE
fi

$deplo input=inputs  trim=@trim.txt > tmp/x5.dot
if diff tmp/x5.dot outputs/with-trim.dot ; then
    echo SUCCESS;
else
    echo FAILURE
fi
