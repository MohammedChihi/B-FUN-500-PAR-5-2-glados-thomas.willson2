#!/bin/bash

result=$(./glados files/call.scm)

if [ $result == "5" ]; then
    echo "The result is equal to 5."
else
    echo "The result is not equal to 5."
fi

result=$(./glados files/lambda1.scm)

if [ "$result" == "#<procedure>" ]; then
    echo "The result is equal to #<procedure>."
else
    echo "The result is not equal to #<procedure>."
fi

result=$(./glados files/lambda2.scm)

if [ "$result" == "3" ]; then
    echo "The result is equal to 3."
else
    echo "The result is not equal to 3."
fi

result=$(./glados files/lambda3.scm)

if [ "$result" == "7" ]; then
    echo "The result is equal to 7."
else
    echo "The result is not equal to 7."
fi

result=$(./glados files/if1.scm)

if [ "$result" == "1" ]; then
    echo "The result is equal to 1."
else
    echo "The result is not equal to 1."
fi

result=$(./glados files/if2.scm)

if [ "$result" == "2" ]; then
    echo "The result is equal to 2."
else
    echo "The result is not equal to 2."
fi

result=$(./glados files/if3.scm)

if [ "$result" == "21" ]; then
    echo "The result is equal to 21."
else
    echo "The result is not equal to 21."
fi

result=$(./glados files/builtins1.scm)

if [ "$result" == "11" ]; then
    echo "The result is equal to 11."
else
    echo "The result is not equal to 11."
fi

result=$(./glados files/builtins2.scm)

if [ "$result" == "#t" ]; then
    echo "The result is equal to #t."
else
    echo "The result is not equal to #t."
fi

result=$(./glados files/builtins3.scm)

if [ "$result" == "#f" ]; then
    echo "The result is equal to #f."
else
    echo "The result is not equal to #f."
fi

result=$(./glados files/superior.scm)

if [ "$result" == "#t" ]; then
    echo "The result is equal to #t."
else
    echo "The result is not equal to #t."
fi

result=$(./glados files/factorial.scm)

if [ "$result" == "3628800" ]; then
    echo "The result is equal to 3628800."
else
    echo "The result is not equal to 3628800."
fi
