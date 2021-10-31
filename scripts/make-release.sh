#!/bin/sh
mkdir dist || echo exists
cp result/bin/* dist
chmod +w dist/*
patchelf --set-interpreter /lib64/ld-linux.so.2 dist/*
gh release create v0.1.0.0 --notes "Initial release" dist/
rm -rf dist