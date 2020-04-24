#!/bin/bash
toin() {
    # Convert in to new in
    cat | sed "s/asl/I\'m a 28 year old guy from England./g" \
        | sed "s/hi/Hello, I'm a 28 year old guy from England./g" \
        | sed "s/intro/I code - and it's also my job./g"
}
toout() {
    # When in, send out
    cat | sed "s/asl/I\'m a 28 year old guy from England./g" \
        | sed 's/Type\s+the\s+word\s+"(.*)/$1/g'
}
toexit() {
    cat
}